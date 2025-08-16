# Data Setup for Stretch Tracker App
# This file handles data initialization and management

# Create initial stretch dataset from CSV file
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
    
    # Create the stretches dataframe
    stretches <- data.frame(
      id = 1:nrow(csv_data),
      name = csv_data$Stretch,
      priority = tolower(csv_data$Priority),
      category = categorize_stretch(csv_data$Stretch),
      description = descriptions[1:nrow(csv_data)],
      stringsAsFactors = FALSE
    )
    
    return(stretches)
  } else {
    # Fallback if CSV doesn't exist
    stop("list.csv file not found. Please ensure the file exists in the app directory.")
  }
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
  # Create data directory if it doesn't exist
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # Create stretches dataset if it doesn't exist
  if (!file.exists("data/stretches.rds")) {
    stretches <- create_stretch_dataset()
    saveRDS(stretches, "data/stretches.rds")
  }
  
  # Create empty daily stats if it doesn't exist
  if (!file.exists("data/daily_stats.rds")) {
    daily_stats <- data.frame(
      date = as.Date(character()),
      completed_count = integer(),
      skipped_count = integer(),
      total_count = integer(),
      stringsAsFactors = FALSE
    )
    saveRDS(daily_stats, "data/daily_stats.rds")
  }
  
  # Create empty stretch history if it doesn't exist
  if (!file.exists("data/stretch_history.rds")) {
    stretch_history <- data.frame(
      id = integer(),
      stretch_id = integer(),
      stretch_name = character(),
      action = character(),
      timestamp = as.POSIXct(character()),
      date = as.Date(character()),
      stringsAsFactors = FALSE
    )
    saveRDS(stretch_history, "data/stretch_history.rds")
  }
  
  # Create user preferences if it doesn't exist
  if (!file.exists("data/user_preferences.rds")) {
    user_prefs <- list(
      daily_goal = 5,
      high_priority_weight = 3,
      low_priority_weight = 1,
      recency_weight = 2,
      never_done_bonus = 5
    )
    saveRDS(user_prefs, "data/user_preferences.rds")
  }
}

# Load functions
load_stretches_data <- function() {
  if (file.exists("data/stretches.rds")) {
    return(readRDS("data/stretches.rds"))
  } else {
    return(create_stretch_dataset())
  }
}

load_daily_stats <- function() {
  if (file.exists("data/daily_stats.rds")) {
    return(readRDS("data/daily_stats.rds"))
  } else {
    return(data.frame(
      date = as.Date(character()),
      completed_count = integer(),
      skipped_count = integer(),
      total_count = integer(),
      stringsAsFactors = FALSE
    ))
  }
}

load_stretch_history <- function() {
  if (file.exists("data/stretch_history.rds")) {
    return(readRDS("data/stretch_history.rds"))
  } else {
    return(data.frame(
      id = integer(),
      stretch_id = integer(),
      stretch_name = character(),
      action = character(),
      timestamp = as.POSIXct(character()),
      date = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }
}

load_user_preferences <- function() {
  if (file.exists("data/user_preferences.rds")) {
    return(readRDS("data/user_preferences.rds"))
  } else {
    return(list(
      daily_goal = 5,
      high_priority_weight = 3,
      low_priority_weight = 1,
      recency_weight = 2,
      never_done_bonus = 5
    ))
  }
}

# Save functions
save_daily_stats <- function(daily_stats) {
  saveRDS(daily_stats, "data/daily_stats.rds")
}

save_stretch_history <- function(stretch_history) {
  saveRDS(stretch_history, "data/stretch_history.rds")
}

# Record stretch action
record_stretch_action <- function(stretch_id, action) {
  # Load current data
  stretch_history <- load_stretch_history()
  daily_stats <- load_daily_stats()
  stretches <- load_stretches_data()
  
  # Get stretch name
  stretch_name <- stretches$name[stretches$id == stretch_id]
  
  # Add to stretch history
  new_record <- data.frame(
    id = ifelse(nrow(stretch_history) == 0, 1, max(stretch_history$id) + 1),
    stretch_id = stretch_id,
    stretch_name = stretch_name,
    action = action,
    timestamp = Sys.time(),
    date = Sys.Date(),
    stringsAsFactors = FALSE
  )
  
  stretch_history <- rbind(stretch_history, new_record)
  save_stretch_history(stretch_history)
  
  # Update daily stats
  today <- Sys.Date()
  today_stats <- daily_stats[daily_stats$date == today, ]
  
  if (nrow(today_stats) == 0) {
    # Create new day record
    new_day <- data.frame(
      date = today,
      completed_count = ifelse(action == "completed", 1, 0),
      skipped_count = ifelse(action == "skipped", 1, 0),
      total_count = 1,
      stringsAsFactors = FALSE
    )
    daily_stats <- rbind(daily_stats, new_day)
  } else {
    # Update existing day record
    if (action == "completed") {
      daily_stats$completed_count[daily_stats$date == today] <- 
        daily_stats$completed_count[daily_stats$date == today] + 1
    } else if (action == "skipped") {
      daily_stats$skipped_count[daily_stats$date == today] <- 
        daily_stats$skipped_count[daily_stats$date == today] + 1
    }
    daily_stats$total_count[daily_stats$date == today] <- 
      daily_stats$total_count[daily_stats$date == today] + 1
  }
  
  save_daily_stats(daily_stats)
}

# Reset all data
reset_all_data <- function() {
  if (file.exists("data/daily_stats.rds")) {
    file.remove("data/daily_stats.rds")
  }
  if (file.exists("data/stretch_history.rds")) {
    file.remove("data/stretch_history.rds")
  }
  initialize_data()
}

# CRUD Operations for Stretch Management

# Add new stretch - completely rewritten with a simpler approach
add_new_stretch <- function(name, priority, category, description, enabled = TRUE) {
  cat("add_new_stretch function called\n")
  cat("Parameters - Name:", name,
      "Priority:", priority,
      "Category:", category,
      "Enabled:", enabled, "\n")
  cat("Description length:", nchar(description), "\n")
  
  tryCatch({
    # Load current stretches
    cat("Loading current stretches data\n")
    stretches <- load_stretches_data()
    cat("Current stretches count:", nrow(stretches), "\n")
    
    # Check if stretch name already exists
    if (name %in% stretches$name) {
      cat("Error: Stretch name already exists\n")
      return(list(success = FALSE, message = "A stretch with this name already exists."))
    }
    
    # Create new stretch ID
    new_id <- ifelse(nrow(stretches) == 0, 1, max(stretches$id) + 1)
    cat("New stretch ID:", new_id, "\n")
    
    # Create a basic data frame with just the essential columns
    new_stretch <- data.frame(
      id = new_id,
      name = name,
      priority = priority,
      category = category,
      description = description,
      enabled = enabled,
      stringsAsFactors = FALSE
    )
    
    # Convert to list for easier manipulation
    stretch_list <- as.list(new_stretch)
    
    # Get all column names from existing stretches
    all_cols <- names(stretches)
    cat("All columns in stretches:", paste(all_cols, collapse=", "), "\n")
    
    # Create a new list with all columns from the original dataframe
    new_row <- list()
    for (col in all_cols) {
      if (col %in% names(stretch_list)) {
        # Use the value we provided
        new_row[[col]] <- stretch_list[[col]]
      } else {
        # Use NA for any other columns
        new_row[[col]] <- NA
      }
    }
    
    # Add the new row directly to the dataframe
    cat("Adding new row to stretches dataframe\n")
    stretches[nrow(stretches) + 1, ] <- new_row
    cat("Updated stretches count:", nrow(stretches), "\n")
    
    # Save to file
    cat("Saving to file: data/stretches.rds\n")
    saveRDS(stretches, "data/stretches.rds")
    cat("Save completed\n")
    
    return(list(success = TRUE, message = "Stretch added successfully"))
    
  }, error = function(e) {
    cat("Error in add_new_stretch:", e$message, "\n")
    return(list(success = FALSE, message = paste("Error adding stretch:", e$message)))
  })
}

# Update existing stretch
update_stretch <- function(id, name, priority, category, description, enabled = TRUE) {
  tryCatch({
    # Load current stretches
    stretches <- load_stretches_data()
    
    # Find the stretch to update
    stretch_index <- which(stretches$id == id)
    if (length(stretch_index) == 0) {
      return(list(success = FALSE, message = "Stretch not found."))
    }
    
    # Check if new name conflicts with existing stretch (excluding current one)
    existing_names <- stretches$name[stretches$id != id]
    if (name %in% existing_names) {
      return(list(success = FALSE, message = "A stretch with this name already exists."))
    }
    
    # Update the stretch
    stretches$name[stretch_index] <- name
    stretches$priority[stretch_index] <- priority
    stretches$category[stretch_index] <- category
    stretches$description[stretch_index] <- description
    stretches$enabled[stretch_index] <- enabled
    
    # Save to file
    saveRDS(stretches, "data/stretches.rds")
    
    return(list(success = TRUE, message = "Stretch updated successfully"))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error updating stretch:", e$message)))
  })
}

# Delete stretch
delete_stretch <- function(id) {
  tryCatch({
    # Load current stretches
    stretches <- load_stretches_data()
    
    # Find the stretch to delete
    stretch_index <- which(stretches$id == id)
    if (length(stretch_index) == 0) {
      return(list(success = FALSE, message = "Stretch not found."))
    }
    
    # Check if stretch has history (optional warning)
    stretch_history <- load_stretch_history()
    has_history <- any(stretch_history$stretch_id == id)
    
    # Remove the stretch
    stretches <- stretches[-stretch_index, ]
    
    # Save to file
    saveRDS(stretches, "data/stretches.rds")
    
    # Optionally clean up history for deleted stretch
    if (has_history) {
      stretch_history <- stretch_history[stretch_history$stretch_id != id, ]
      save_stretch_history(stretch_history)
    }
    
    return(list(success = TRUE, message = "Stretch deleted successfully"))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error deleting stretch:", e$message)))
  })
}

# Get stretch by ID
get_stretch_by_id <- function(id) {
  stretches <- load_stretches_data()
  stretch_index <- which(stretches$id == id)
  if (length(stretch_index) == 0) {
    return(NULL)
  }
  return(stretches[stretch_index, ])
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