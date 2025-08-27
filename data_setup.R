# Data Setup for Stretch Tracker App
# This file handles data initialization and management

# Load required database libraries
library(DBI)
library(RSQLite)

# Database connection and management functions
get_db_connection <- function() {
  # Create data directory if it doesn't exist
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # Connect to SQLite database
  con <- dbConnect(RSQLite::SQLite(), "data/stretch_tracker.sqlite")
  return(con)
}

# Initialize database tables
initialize_database_tables <- function() {
  con <- get_db_connection()
  
  # Create stretches table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stretches (
      id INTEGER PRIMARY KEY,
      name TEXT NOT NULL UNIQUE,
      priority TEXT NOT NULL CHECK (priority IN ('high', 'low')),
      category TEXT NOT NULL,
      description TEXT NOT NULL,
      enabled BOOLEAN NOT NULL DEFAULT 1
    )
  ")
  
  # Create daily_stats table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS daily_stats (
      date DATE PRIMARY KEY,
      completed_count INTEGER NOT NULL DEFAULT 0,
      skipped_count INTEGER NOT NULL DEFAULT 0,
      total_count INTEGER NOT NULL DEFAULT 0
    )
  ")
  
  # Create stretch_history table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stretch_history (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      stretch_id INTEGER NOT NULL,
      stretch_name TEXT NOT NULL,
      action TEXT NOT NULL CHECK (action IN ('completed', 'skipped')),
      timestamp DATETIME NOT NULL,
      date DATE NOT NULL,
      FOREIGN KEY (stretch_id) REFERENCES stretches (id)
    )
  ")
  
  # Create user_preferences table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_preferences (
      key TEXT PRIMARY KEY,
      value REAL NOT NULL
    )
  ")
  
  # Insert default user preferences if they don't exist
  default_prefs <- list(
    daily_goal = 5,
    high_priority_weight = 3,
    low_priority_weight = 1,
    recency_weight = 2,
    never_done_bonus = 5
  )
  
  for (key in names(default_prefs)) {
    dbExecute(con, "INSERT OR IGNORE INTO user_preferences (key, value) VALUES (?, ?)",
              params = list(key, default_prefs[[key]]))
  }
  
  dbDisconnect(con)
}

# Create initial stretch dataset from CSV file and insert into database
create_stretch_dataset <- function() {
  # Read the CSV file
  if (file.exists("list.csv")) {
    csv_data <- read.csv("list.csv", stringsAsFactors = FALSE)
    
    # Create descriptions for each stretch
    descriptions <- c(
      "Strengthen glutes and hamstrings while engaging core",
      "Stretch hip flexors with coaching guidance for proper form",
      "Advanced yoga pose that strengthens shoulders and opens chest",
      "Essential stretch for tight hip flexors from sitting",
      "Use foam roller to release tension in hamstring muscles",
      "Dynamic movement combining plank with cardio element",
      "Stretch calf muscles to improve ankle mobility",
      "Roll glute muscles to release tension and improve mobility",
      "Core exercise that challenges stability and coordination",
      "Gentle yoga pose that opens hips and releases lower back",
      "Forward fold with shoulder opener for upper body release",
      "Stretch toes and feet by sitting back on heels",
      "Transition from high to low plank for core strength",
      "Tabletop position with toe touches for core activation",
      "Use foam roller to release calf muscle tension",
      "Reverse plank pose that strengthens posterior chain",
      "Quadruped exercise for core stability and coordination",
      "Plank variation with knee-to-elbow movement",
      "Side-lying exercise to strengthen hip abductors",
      "Fundamental movement pattern for leg strength",
      "Hamstring stretch in seated position with forward fold",
      "Intense plank variation for maximum core engagement",
      "Spinal mobility exercise with twisting motion",
      "Single-leg balance exercise for calf strength",
      "Back extension exercise in prone position",
      "Hip stretch in figure-4 position",
      "Hip mobility exercise with lifting motion",
      "Seated spinal rotation for thoracic mobility",
      "Side-stepping exercise with resistance band",
      "Hip abduction exercise with resistance band",
      "Deep squat hold for hip and ankle mobility",
      "Stretch chest muscles to counteract forward posture",
      "Functional movement from lying to standing",
      "Foot and ankle mobility exercise using ball"
    )
    
    # Connect to database
    con <- get_db_connection()
    
    # Insert stretches into database
    for (i in 1:nrow(csv_data)) {
      dbExecute(con, "
        INSERT OR IGNORE INTO stretches (id, name, priority, category, description, enabled)
        VALUES (?, ?, ?, ?, ?, ?)
      ", params = list(
        i,
        csv_data$Stretch[i],
        tolower(csv_data$Priority[i]),
        categorize_stretch(csv_data$Stretch[i]),
        descriptions[i],
        TRUE
      ))
    }
    
    # Get count of inserted stretches
    stretch_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM stretches")$count
    cat("Inserted", stretch_count, "stretches into database\n")
    
    dbDisconnect(con)
    
    return(TRUE)
  } else {
    # Fallback if CSV doesn't exist
    stop("list.csv file not found. Please ensure the file exists in the app directory.")
  }
}

# Add logging to reset_all_data
reset_all_data <- function() {
  if (file.exists("data/daily_stats.rds")) {
    file.remove("data/daily_stats.rds")
  }
  if (file.exists("data/stretch_history.rds")) {
    file.remove("data/stretch_history.rds")
  }
  # Also remove stretches.rds to ensure a clean recreation from list.csv
  if (file.exists("data/stretches.rds")) {
    file.remove("data/stretches.rds")
  }
  initialize_data()
}

# Helper function to categorize stretches
categorize_stretch <- function(stretch_names) {
  categories <- character(length(stretch_names))
  
  for (i in seq_along(stretch_names)) {
    name <- tolower(stretch_names[i])
    
    if (grepl("hip|#4|cook|clamshells|fire hydrants", name)) {
      categories[i] <- "hips"
    } else if (grepl("plank|dead bug|bird dog|superman", name)) {
      categories[i] <- "core"
    } else if (grepl("calf|heel|toe|feet|ball w feet", name)) {
      categories[i] <- "feet_ankles"
    } else if (grepl("spinal|chest|shoulder", name)) {
      categories[i] <- "spine_shoulders"
    } else if (grepl("squat|bridges|get up", name)) {
      categories[i] <- "functional"
    } else if (grepl("roll|monster|walks", name)) {
      categories[i] <- "mobility"
    } else if (grepl("dolphin|happy baby|forward|splits|scorpion", name)) {
      categories[i] <- "flexibility"
    } else {
      categories[i] <- "general"
    }
  }
  
  return(categories)
}

# Initialize all data files
initialize_data <- function() {
  # Initialize database tables
  initialize_database_tables()
  
  # Check if stretches table is empty and populate it if needed
  con <- get_db_connection()
  stretch_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM stretches")$count
  dbDisconnect(con)
  
  if (stretch_count == 0) {
    cat("Stretches table is empty, populating from CSV...\n")
    create_stretch_dataset()
  } else {
    cat("Found", stretch_count, "stretches in database\n")
  }
}

# Load functions
load_stretches_data <- function() {
  cat("load_stretches_data called.\n")
  con <- get_db_connection()
  data <- dbGetQuery(con, "SELECT * FROM stretches ORDER BY id")
  dbDisconnect(con)
  cat("Loaded", nrow(data), "stretches from database\n")
  return(data)
}

load_daily_stats <- function() {
  con <- get_db_connection()
  data <- dbGetQuery(con, "SELECT * FROM daily_stats ORDER BY date")
  dbDisconnect(con)
  
  # Convert date column to Date type
  if (nrow(data) > 0) {
    data$date <- as.Date(data$date)
  }
  
  return(data)
}

load_stretch_history <- function() {
  con <- get_db_connection()
  data <- dbGetQuery(con, "SELECT * FROM stretch_history ORDER BY timestamp DESC")
  dbDisconnect(con)
  
  # Convert timestamp and date columns to proper types
  if (nrow(data) > 0) {
    data$timestamp <- as.POSIXct(data$timestamp)
    data$date <- as.Date(data$date)
  }
  
  return(data)
}

load_user_preferences <- function() {
  con <- get_db_connection()
  prefs_data <- dbGetQuery(con, "SELECT key, value FROM user_preferences")
  dbDisconnect(con)
  
  # Convert to named list
  prefs <- list()
  for (i in 1:nrow(prefs_data)) {
    prefs[[prefs_data$key[i]]] <- prefs_data$value[i]
  }
  
  return(prefs)
}

# Save functions (now update database directly)
save_daily_stats <- function(daily_stats) {
  # This function is kept for compatibility but now updates database directly
  # Individual daily stats updates are handled in record_stretch_action
  cat("save_daily_stats called - database updates are handled automatically\n")
}

save_stretch_history <- function(stretch_history) {
  # This function is kept for compatibility but now updates database directly
  # Individual history updates are handled in record_stretch_action
  cat("save_stretch_history called - database updates are handled automatically\n")
}

# Record stretch action
record_stretch_action <- function(stretch_id, action) {
  con <- get_db_connection()
  
  # Get stretch name
  stretch_name <- dbGetQuery(con, "SELECT name FROM stretches WHERE id = ?", params = list(stretch_id))$name[1]
  
  # Add to stretch history
  dbExecute(con, "
    INSERT INTO stretch_history (stretch_id, stretch_name, action, timestamp, date)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(
    stretch_id,
    stretch_name,
    action,
    as.character(Sys.time()),
    as.character(Sys.Date())
  ))
  
  # Update daily stats
  today <- as.character(Sys.Date())
  
  # Check if today's record exists
  today_exists <- dbGetQuery(con, "SELECT COUNT(*) as count FROM daily_stats WHERE date = ?", params = list(today))$count > 0
  
  if (!today_exists) {
    # Create new day record
    dbExecute(con, "
      INSERT INTO daily_stats (date, completed_count, skipped_count, total_count)
      VALUES (?, ?, ?, ?)
    ", params = list(
      today,
      ifelse(action == "completed", 1, 0),
      ifelse(action == "skipped", 1, 0),
      1
    ))
  } else {
    # Update existing day record
    if (action == "completed") {
      dbExecute(con, "
        UPDATE daily_stats
        SET completed_count = completed_count + 1, total_count = total_count + 1
        WHERE date = ?
      ", params = list(today))
    } else if (action == "skipped") {
      dbExecute(con, "
        UPDATE daily_stats
        SET skipped_count = skipped_count + 1, total_count = total_count + 1
        WHERE date = ?
      ", params = list(today))
    }
  }
  
  dbDisconnect(con)
}

# Reset all data
reset_all_data <- function() {
  con <- get_db_connection()
  
  # Clear all tables
  dbExecute(con, "DELETE FROM stretch_history")
  dbExecute(con, "DELETE FROM daily_stats")
  dbExecute(con, "DELETE FROM stretches")
  dbExecute(con, "DELETE FROM user_preferences")
  
  dbDisconnect(con)
  
  # Reinitialize data
  initialize_data()
}

# CRUD Operations for Stretch Management

# Add new stretch
add_new_stretch <- function(name, priority, category, description, enabled = TRUE) {
  tryCatch({
    con <- get_db_connection()
    
    # Check if stretch name already exists
    existing_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM stretches WHERE name = ?", params = list(name))$count
    if (existing_count > 0) {
      dbDisconnect(con)
      return(list(success = FALSE, message = "A stretch with this name already exists."))
    }
    
    # Get next ID
    max_id <- dbGetQuery(con, "SELECT COALESCE(MAX(id), 0) as max_id FROM stretches")$max_id
    new_id <- max_id + 1
    
    # Insert new stretch
    dbExecute(con, "
      INSERT INTO stretches (id, name, priority, category, description, enabled)
      VALUES (?, ?, ?, ?, ?, ?)
    ", params = list(new_id, name, priority, category, description, enabled))
    
    dbDisconnect(con)
    return(list(success = TRUE, message = "Stretch added successfully"))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error adding stretch:", e$message)))
  })
}

# Update existing stretch
update_stretch <- function(id, name, priority, category, description, enabled = TRUE) {
  tryCatch({
    con <- get_db_connection()
    
    # Check if stretch exists
    stretch_exists <- dbGetQuery(con, "SELECT COUNT(*) as count FROM stretches WHERE id = ?", params = list(id))$count > 0
    if (!stretch_exists) {
      dbDisconnect(con)
      return(list(success = FALSE, message = "Stretch not found."))
    }
    
    # Check if new name conflicts with existing stretch (excluding current one)
    existing_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM stretches WHERE name = ? AND id != ?", params = list(name, id))$count
    if (existing_count > 0) {
      dbDisconnect(con)
      return(list(success = FALSE, message = "A stretch with this name already exists."))
    }
    
    # Update the stretch
    dbExecute(con, "
      UPDATE stretches
      SET name = ?, priority = ?, category = ?, description = ?, enabled = ?
      WHERE id = ?
    ", params = list(name, priority, category, description, enabled, id))
    
    dbDisconnect(con)
    return(list(success = TRUE, message = "Stretch updated successfully"))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error updating stretch:", e$message)))
  })
}

# Delete stretch
delete_stretch <- function(id) {
  tryCatch({
    con <- get_db_connection()
    
    # Check if stretch exists
    stretch_exists <- dbGetQuery(con, "SELECT COUNT(*) as count FROM stretches WHERE id = ?", params = list(id))$count > 0
    if (!stretch_exists) {
      dbDisconnect(con)
      return(list(success = FALSE, message = "Stretch not found."))
    }
    
    # Delete related history records first (foreign key constraint)
    dbExecute(con, "DELETE FROM stretch_history WHERE stretch_id = ?", params = list(id))
    
    # Delete the stretch
    dbExecute(con, "DELETE FROM stretches WHERE id = ?", params = list(id))
    
    dbDisconnect(con)
    return(list(success = TRUE, message = "Stretch deleted successfully"))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error deleting stretch:", e$message)))
  })
}

# Get stretch by ID
get_stretch_by_id <- function(id) {
  con <- get_db_connection()
  stretch <- dbGetQuery(con, "SELECT * FROM stretches WHERE id = ?", params = list(id))
  dbDisconnect(con)
  
  if (nrow(stretch) == 0) {
    return(NULL)
  }
  return(stretch)
}

# Validate stretch data
validate_stretch_data <- function(name, priority, category, description) {
  errors <- character(0)
  
  if (is.null(name) || name == "" || nchar(trimws(name)) == 0) {
    errors <- c(errors, "Stretch name is required")
  }
  
  if (nchar(name) > 100) {
    errors <- c(errors, "Stretch name must be less than 100 characters")
  }
  
  if (!priority %in% c("high", "low")) {
    errors <- c(errors, "Priority must be 'high' or 'low'")
  }
  
  valid_categories <- c("hips", "core", "feet_ankles", "spine_shoulders",
                       "functional", "mobility", "flexibility", "general")
  if (!category %in% valid_categories) {
    errors <- c(errors, "Invalid category selected")
  }
  
  if (is.null(description) || description == "" || nchar(trimws(description)) == 0) {
    errors <- c(errors, "Description is required")
  }
  
  if (nchar(description) > 500) {
    errors <- c(errors, "Description must be less than 500 characters")
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}