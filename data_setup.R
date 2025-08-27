# Data Setup for Stretch Tracker App
# This file handles data initialization and management

cat("Attempting to load googledrive library...\n")
library(googledrive)
# rsconnect library is not strictly needed for environment detection if using R_SHINY_APP_NAME
cat("googledrive library loaded successfully.\n")

# Define the Google Drive folder name for the application data
DRIVE_APP_FOLDER <- "StretchiesAppData"

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
      enabled = TRUE, # Explicitly add enabled column and set to TRUE
      stringsAsFactors = FALSE
    )
    
    print(head(stretches))
    print(tail(stretches))
    print(summary(stretches))
    
    return(stretches)
  } else {
    # Fallback if CSV doesn't exist
    stop("list.csv file not found. Please ensure the file exists in the app directory.")
  }
}

reset_all_data <- function() {
  cat("Resetting all data...\n")
  
  # Delete files from Google Drive
  files_to_delete <- c("daily_stats.rds", "stretch_history.rds", "stretches.rds", "user_preferences.rds")
  app_folder <- drive_find(DRIVE_APP_FOLDER, type = "folder", verbose = FALSE)
  
  if (nrow(app_folder) > 0) {
    for (file_name in files_to_delete) {
      file_path_gd <- file.path(DRIVE_APP_FOLDER, file_name)
      if (nrow(drive_find(file_path_gd, verbose = FALSE)) > 0) {
        cat("Deleting", file_name, "from Google Drive.\n")
        drive_rm(drive_get(file_path_gd), verbose = FALSE)
      }
    }
  } else {
    cat("Google Drive application folder not found. No files to delete from Drive.\n")
  }
  
  # Re-initialize data, which will create new empty files in Google Drive
  initialize_data()
  cat("Data reset complete and re-initialized.\n")
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
  cat("Starting Google Drive authentication...\n")
  # Authenticate with Google Drive (non-interactive for shinyapps.io)
  # This assumes GOOGLEDrive_TOKEN environment variable is set on shinyapps.io
  # or a .httr-oauth file is present.
  # For local development, it will prompt for interactive authentication.
  # Authenticate with Google Drive
  # Authenticate with Google Drive
  tryCatch({
    # Debugging environment variables
    cat("DEBUG: R_SHINY_APP_NAME:", Sys.getenv("R_SHINY_APP_NAME"), "\n")
    cat("DEBUG: rsconnect.shinyapps.io.account option:", getOption("rsconnect.shinyapps.io.account"), "\n")

    # Check if running on shinyapps.io using a more reliable method
    if (!is.null(getOption("rsconnect.shinyapps.io.account")) || Sys.getenv("R_SHINY_APP_NAME") != "") {
      cat("DEBUG: Detected shinyapps.io environment.\n")
      token_value <- Sys.getenv("GOOGLEDrive_TOKEN")
      if (token_value != "") {
        cat("DEBUG: GOOGLEDrive_TOKEN found. Attempting drive_auth with token...\n")
        drive_auth(token = token_value)
        if (drive_has_token()) {
          cat("Google Drive authentication successful using GOOGLEDrive_TOKEN on shinyapps.io.\n")
        } else {
          stop("drive_auth with GOOGLEDrive_TOKEN failed to acquire a token on shinyapps.io.")
        }
      } else {
        stop("GOOGLEDrive_TOKEN environment variable is not set on shinyapps.io. Google Drive authentication failed.")
      }
    } else {
      # Local development: attempt interactive/cached authentication
      cat("DEBUG: Running locally. Attempting interactive/cached auth...\n")
      drive_auth(cache = ".httr-oauth", email = TRUE)
      if (drive_has_token()) {
        cat("Google Drive authentication successful using cached token or interactive auth locally.\n")
      } else {
        stop("Local Google Drive authentication failed. Please ensure you can interactively authenticate or .httr-oauth is present.")
      }
    }
  }, error = function(e) {
    cat(paste0("ERROR: Google Drive authentication failed in tryCatch block: ", e$message, "\n"))
    stop(paste0("Google Drive authentication failed. Please ensure GOOGLEDrive_TOKEN is set on shinyapps.io or .httr-oauth is present locally. Error: ", e$message))
  }, warning = function(w) {
    cat(paste0("WARNING: Google Drive authentication warning: ", w$message, "\n"))
  })
  
  # Check for and create the application folder in Google Drive
  cat("Checking for Google Drive application folder...\n")
  app_folder <- drive_find(DRIVE_APP_FOLDER, type = "folder", verbose = FALSE)
  if (nrow(app_folder) == 0) {
    cat("Google Drive folder '", DRIVE_APP_FOLDER, "' not found. Creating it...\n")
    app_folder <- drive_mkdir(DRIVE_APP_FOLDER, verbose = FALSE)
    cat("Google Drive folder created successfully.\n")
  } else {
    cat("Google Drive folder '", DRIVE_APP_FOLDER, "' already exists.\n")
  }
  
  # Define file paths within the Google Drive folder
  stretches_path_gd <- file.path(DRIVE_APP_FOLDER, "stretches.rds")
  daily_stats_path_gd <- file.path(DRIVE_APP_FOLDER, "daily_stats.rds")
  stretch_history_path_gd <- file.path(DRIVE_APP_FOLDER, "stretch_history.rds")
  user_preferences_path_gd <- file.path(DRIVE_APP_FOLDER, "user_preferences.rds")
  
  # Check and create/upload initial data files if they don't exist in Google Drive
  
  # Stretches data
  cat("Checking for stretches.rds in Google Drive...\n")
  if (nrow(drive_find(stretches_path_gd, verbose = FALSE)) == 0) {
    cat("stretches.rds not found in Google Drive. Creating and uploading initial dataset.\n")
    stretches <- create_stretch_dataset()
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(stretches, temp_file)
    drive_upload(temp_file, path = as_id(app_folder), name = "stretches.rds", verbose = FALSE)
    unlink(temp_file)
    cat("stretches.rds uploaded successfully.\n")
  } else {
    cat("stretches.rds found in Google Drive.\n")
  }
  
  # Daily stats
  cat("Checking for daily_stats.rds in Google Drive...\n")
  if (nrow(drive_find(daily_stats_path_gd, verbose = FALSE)) == 0) {
    cat("daily_stats.rds not found in Google Drive. Creating and uploading initial dataset.\n")
    daily_stats <- data.frame(
      date = as.Date(character()),
      completed_count = integer(),
      skipped_count = integer(),
      total_count = integer(),
      stringsAsFactors = FALSE
    )
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(daily_stats, temp_file)
    drive_upload(temp_file, path = as_id(app_folder), name = "daily_stats.rds", verbose = FALSE)
    unlink(temp_file)
    cat("daily_stats.rds uploaded successfully.\n")
  } else {
    cat("daily_stats.rds found in Google Drive.\n")
  }
  
  # Stretch history
  cat("Checking for stretch_history.rds in Google Drive...\n")
  if (nrow(drive_find(stretch_history_path_gd, verbose = FALSE)) == 0) {
    cat("stretch_history.rds not found in Google Drive. Creating and uploading initial dataset.\n")
    stretch_history <- data.frame(
      id = integer(),
      stretch_id = integer(),
      stretch_name = character(),
      action = character(),
      timestamp = as.POSIXct(character()),
      date = as.Date(character()),
      stringsAsFactors = FALSE
    )
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(stretch_history, temp_file)
    drive_upload(temp_file, path = as_id(app_folder), name = "stretch_history.rds", verbose = FALSE)
    unlink(temp_file)
    cat("stretch_history.rds uploaded successfully.\n")
  } else {
    cat("stretch_history.rds found in Google Drive.\n")
  }
  
  # User preferences
  cat("Checking for user_preferences.rds in Google Drive...\n")
  if (nrow(drive_find(user_preferences_path_gd, verbose = FALSE)) == 0) {
    cat("user_preferences.rds not found in Google Drive. Creating and uploading initial dataset.\n")
    user_prefs <- list(
      daily_goal = 5,
      high_priority_weight = 3,
      low_priority_weight = 1,
      recency_weight = 2,
      never_done_bonus = 5
    )
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(user_prefs, temp_file)
    drive_upload(temp_file, path = as_id(app_folder), name = "user_preferences.rds", verbose = FALSE)
    unlink(temp_file)
    cat("user_preferences.rds uploaded successfully.\n")
  } else {
    cat("user_preferences.rds found in Google Drive.\n")
  }
}

# Load functions
load_stretches_data <- function() {
  cat("load_stretches_data called. Checking for stretches.rds...\n")
  file_path_gd <- file.path(DRIVE_APP_FOLDER, "stretches.rds")
  
  if (nrow(drive_find(file_path_gd, verbose = FALSE)) > 0) {
    cat("stretches.rds found in Google Drive. Reading data...\n")
    temp_file <- tempfile(fileext = ".rds")
    drive_download(drive_get(file_path_gd), path = temp_file, overwrite = TRUE, verbose = FALSE)
    data <- readRDS(temp_file)
    unlink(temp_file)
    cat("Loaded stretches from Google Drive. Rows:", nrow(data), "\n")
    return(data)
  } else {
    cat("stretches.rds not found in Google Drive during load. Attempting to create new dataset locally and upload.\n")
    data <- create_stretch_dataset()
    # Upload the newly created data to Google Drive
    app_folder <- drive_find(DRIVE_APP_FOLDER, type = "folder", verbose = FALSE)
    if (nrow(app_folder) > 0) {
      temp_file <- tempfile(fileext = ".rds")
      saveRDS(data, temp_file)
      drive_upload(temp_file, path = as_id(app_folder), name = "stretches.rds", verbose = FALSE)
      unlink(temp_file)
      cat("Created and uploaded new stretches dataset. Rows:", nrow(data), "\n")
    } else {
      stop("Google Drive application folder not found. Cannot upload initial data.")
    }
    return(data)
  }
}

load_daily_stats <- function() {
  file_path_gd <- file.path(DRIVE_APP_FOLDER, "daily_stats.rds")
  if (nrow(drive_find(file_path_gd, verbose = FALSE)) > 0) {
    temp_file <- tempfile(fileext = ".rds")
    drive_download(drive_get(file_path_gd), path = temp_file, overwrite = TRUE, verbose = FALSE)
    data <- readRDS(temp_file)
    unlink(temp_file)
    return(data)
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
  file_path_gd <- file.path(DRIVE_APP_FOLDER, "stretch_history.rds")
  if (nrow(drive_find(file_path_gd, verbose = FALSE)) > 0) {
    temp_file <- tempfile(fileext = ".rds")
    drive_download(drive_get(file_path_gd), path = temp_file, overwrite = TRUE, verbose = FALSE)
    data <- readRDS(temp_file)
    unlink(temp_file)
    return(data)
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
  file_path_gd <- file.path(DRIVE_APP_FOLDER, "user_preferences.rds")
  if (nrow(drive_find(file_path_gd, verbose = FALSE)) > 0) {
    temp_file <- tempfile(fileext = ".rds")
    drive_download(drive_get(file_path_gd), path = temp_file, overwrite = TRUE, verbose = FALSE)
    data <- readRDS(temp_file)
    unlink(temp_file)
    return(data)
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
  file_path_gd <- file.path(DRIVE_APP_FOLDER, "daily_stats.rds")
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(daily_stats, temp_file)
  drive_update(file = drive_get(file_path_gd), media = temp_file, verbose = FALSE)
  unlink(temp_file)
}

save_stretch_history <- function(stretch_history) {
  file_path_gd <- file.path(DRIVE_APP_FOLDER, "stretch_history.rds")
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(stretch_history, temp_file)
  drive_update(file = drive_get(file_path_gd), media = temp_file, verbose = FALSE)
  unlink(temp_file)
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
  cat("Resetting all data...\n")
  
  # Delete files from Google Drive
  files_to_delete <- c("daily_stats.rds", "stretch_history.rds", "stretches.rds", "user_preferences.rds")
  app_folder <- drive_find(DRIVE_APP_FOLDER, type = "folder", verbose = FALSE)
  
  if (nrow(app_folder) > 0) {
    for (file_name in files_to_delete) {
      file_path_gd <- file.path(DRIVE_APP_FOLDER, file_name)
      if (nrow(drive_find(file_path_gd, verbose = FALSE)) > 0) {
        cat("Deleting", file_name, "from Google Drive.\n")
        drive_rm(drive_get(file_path_gd), verbose = FALSE)
      }
    }
  } else {
    cat("Google Drive application folder not found. No files to delete from Drive.\n")
  }
  
  # Re-initialize data, which will create new empty files in Google Drive
  initialize_data()
  cat("Data reset complete and re-initialized.\n")
}

# CRUD Operations for Stretch Management

# Add new stretch - completely rewritten with a simpler approach
add_new_stretch <- function(name, priority, category, description, enabled = TRUE) {
  tryCatch({
    # Load current stretches
    stretches <- load_stretches_data()
    
    # Check if stretch name already exists
    if (name %in% stretches$name) {
      return(list(success = FALSE, message = "A stretch with this name already exists."))
    }
    
    # Create new stretch ID
    new_id <- ifelse(nrow(stretches) == 0, 1, max(stretches$id) + 1)
    
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
    stretches[nrow(stretches) + 1, ] <- new_row
    
    # Save to Google Drive
    file_path_gd <- file.path(DRIVE_APP_FOLDER, "stretches.rds")
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(stretches, temp_file)
    drive_update(file = drive_get(file_path_gd), media = temp_file, verbose = FALSE)
    unlink(temp_file)
    
    return(list(success = TRUE, message = "Stretch added successfully"))
    
  }, error = function(e) {
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
    
    # Save to Google Drive
    file_path_gd <- file.path(DRIVE_APP_FOLDER, "stretches.rds")
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(stretches, temp_file)
    drive_update(file = drive_get(file_path_gd), media = temp_file, verbose = FALSE)
    unlink(temp_file)
    
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
    
    # Save to Google Drive
    file_path_gd <- file.path(DRIVE_APP_FOLDER, "stretches.rds")
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(stretches, temp_file)
    drive_update(file = drive_get(file_path_gd), media = temp_file, verbose = FALSE)
    unlink(temp_file)
    
    # Optionally clean up history for deleted stretch
    if (has_history) {
      stretch_history <- stretch_history[stretch_history$stretch_id != id, ]
      save_stretch_history(stretch_history) # This will now save to Google Drive
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