# Google Sheets Integration Helper Functions
# This file contains functions to interact with Google Sheets for data storage

library(googlesheets4)
library(googledrive)
library(httr)
library(jsonlite)

# Global variables for Google Sheets
SHEETS_CONFIG <- list(
  stretches_sheet_id = NULL,
  daily_stats_sheet_id = NULL,
  stretch_history_sheet_id = NULL,
  user_preferences_sheet_id = NULL,
  authenticated = FALSE
)

#' Initialize Google Sheets authentication
#' @param service_account_path Path to the service-account.json file
#' @param client_secret_path Path to the client_secret.json file (for local dev)
initialize_google_auth <- function(service_account_path = "service-account.json",
                                   client_secret_path = "client_secret.json") {
  tryCatch({
    # Check if running on shinyapps.io or similar server environment
    is_server <- Sys.getenv("SHINY_SERVER_VERSION") != "" ||
                 Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps" ||
                 !interactive()
    
    if (is_server) {
      cat("Running on server - using service account authentication\n")
      
      # For shinyapps.io, use service account authentication
      if (file.exists(service_account_path)) {
        cat("Found service account file:", service_account_path, "\n")
        
        # Clear any existing authentication
        gs4_deauth()
        drive_deauth()
        
        # Authenticate with service account
        gs4_auth(path = service_account_path)
        drive_auth(path = service_account_path)
        
        SHEETS_CONFIG$authenticated <<- TRUE
        cat("Service account authentication successful!\n")
        return(TRUE)
        
      } else if (file.exists(client_secret_path)) {
        cat("Service account not found, trying OAuth with client secret\n")
        
        # Fallback to OAuth (may not work on server)
        options(
          gargle_oauth_cache = FALSE,
          gargle_oauth_email = FALSE,
          gargle_oob_default = TRUE
        )
        
        gs4_deauth()
        drive_deauth()
        
        gs4_auth(path = client_secret_path, cache = FALSE)
        drive_auth(path = client_secret_path, cache = FALSE)
        
        SHEETS_CONFIG$authenticated <<- TRUE
        cat("OAuth authentication successful!\n")
        return(TRUE)
        
      } else {
        cat("Neither service-account.json nor client_secret.json found\n")
        return(FALSE)
      }
      
    } else {
      cat("Running locally - using interactive OAuth authentication\n")
      
      # For local development, prefer OAuth
      if (file.exists(client_secret_path)) {
        cat("Using client secret for local development\n")
        
        options(
          gargle_oauth_cache = ".secrets",
          gargle_oauth_email = TRUE
        )
        
        gs4_auth(path = client_secret_path, cache = TRUE)
        drive_auth(path = client_secret_path, cache = TRUE)
        
        SHEETS_CONFIG$authenticated <<- TRUE
        cat("Local OAuth authentication successful!\n")
        return(TRUE)
        
      } else if (file.exists(service_account_path)) {
        cat("Using service account for local development\n")
        
        gs4_deauth()
        drive_deauth()
        gs4_auth(path = service_account_path)
        drive_auth(path = service_account_path)
        
        SHEETS_CONFIG$authenticated <<- TRUE
        cat("Local service account authentication successful!\n")
        return(TRUE)
        
      } else {
        cat("No authentication files found\n")
        return(FALSE)
      }
    }
  }, error = function(e) {
    cat("Google Sheets authentication failed:", e$message, "\n")
    cat("Error details:", toString(e), "\n")
    return(FALSE)
  })
}

#' Create or get Google Sheets for the app
#' @param force_create Whether to force creation of new sheets
setup_google_sheets <- function(force_create = FALSE) {
  if (!SHEETS_CONFIG$authenticated) {
    cat("Not authenticated with Google Sheets\n")
    return(FALSE)
  }
  
  tryCatch({
    # Create or find the main spreadsheet
    sheet_name <- "Stretch_Tracker_Data"
    
    # Check if spreadsheet already exists
    existing_sheets <- drive_find(name = sheet_name, type = "spreadsheet")
    
    if (nrow(existing_sheets) > 0 && !force_create) {
      main_sheet_id <- existing_sheets$id[1]
      cat("Found existing spreadsheet:", main_sheet_id, "\n")
    } else {
      # Create new spreadsheet
      main_sheet <- gs4_create(name = sheet_name)
      main_sheet_id <- main_sheet
      cat("Created new spreadsheet:", main_sheet_id, "\n")
    }
    
    # Set up individual sheets within the spreadsheet
    setup_individual_sheets(main_sheet_id)
    
    return(TRUE)
  }, error = function(e) {
    cat("Error setting up Google Sheets:", e$message, "\n")
    return(FALSE)
  })
}

#' Set up individual sheets within the main spreadsheet
setup_individual_sheets <- function(spreadsheet_id) {
  # Get existing sheet names
  sheet_info <- sheet_names(spreadsheet_id)
  
  # Define required sheets and their structures
  required_sheets <- list(
    "stretches" = c("id", "name", "priority", "category", "description", "enabled"),
    "daily_stats" = c("date", "completed_count", "skipped_count", "streak"),
    "stretch_history" = c("id", "stretch_id", "action", "timestamp", "date"),
    "user_preferences" = c("key", "value")
  )
  
  for (sheet_name in names(required_sheets)) {
    if (!sheet_name %in% sheet_info) {
      # Create the sheet
      sheet_add(spreadsheet_id, sheet = sheet_name)
      
      # Add headers
      headers <- required_sheets[[sheet_name]]
      range_write(spreadsheet_id, 
                 data = as.data.frame(t(headers)), 
                 sheet = sheet_name, 
                 range = "A1", 
                 col_names = FALSE)
      
      cat("Created sheet:", sheet_name, "\n")
    }
  }
  
  # Store the spreadsheet ID for later use
  SHEETS_CONFIG$main_sheet_id <<- spreadsheet_id
}

#' Read data from Google Sheets
#' @param sheet_name Name of the sheet to read from
#' @param spreadsheet_id ID of the spreadsheet (uses main if not specified)
read_from_sheets <- function(sheet_name, spreadsheet_id = NULL) {
  if (!SHEETS_CONFIG$authenticated) {
    return(NULL)
  }
  
  if (is.null(spreadsheet_id)) {
    spreadsheet_id <- SHEETS_CONFIG$main_sheet_id
  }
  
  tryCatch({
    data <- read_sheet(spreadsheet_id, sheet = sheet_name)
    return(as.data.frame(data))
  }, error = function(e) {
    cat("Error reading from sheet", sheet_name, ":", e$message, "\n")
    return(NULL)
  })
}

#' Write data to Google Sheets
#' @param data Data frame to write
#' @param sheet_name Name of the sheet to write to
#' @param spreadsheet_id ID of the spreadsheet (uses main if not specified)
#' @param append Whether to append data or overwrite
write_to_sheets <- function(data, sheet_name, spreadsheet_id = NULL, append = FALSE) {
  if (!SHEETS_CONFIG$authenticated) {
    return(FALSE)
  }
  
  if (is.null(spreadsheet_id)) {
    spreadsheet_id <- SHEETS_CONFIG$main_sheet_id
  }
  
  tryCatch({
    if (append) {
      sheet_append(spreadsheet_id, data, sheet = sheet_name)
    } else {
      range_write(spreadsheet_id, data, sheet = sheet_name, range = "A1")
    }
    return(TRUE)
  }, error = function(e) {
    cat("Error writing to sheet", sheet_name, ":", e$message, "\n")
    return(FALSE)
  })
}

#' Sync local SQLite data to Google Sheets
sync_to_sheets <- function() {
  if (!SHEETS_CONFIG$authenticated) {
    cat("Not authenticated with Google Sheets - skipping sync\n")
    return(FALSE)
  }
  
  tryCatch({
    # Read local data
    stretches <- load_stretches_data()
    daily_stats <- load_daily_stats()
    stretch_history <- load_stretch_history()
    
    # Write to Google Sheets
    write_to_sheets(stretches, "stretches")
    write_to_sheets(daily_stats, "daily_stats")
    write_to_sheets(stretch_history, "stretch_history")
    
    cat("Successfully synced data to Google Sheets\n")
    return(TRUE)
  }, error = function(e) {
    cat("Error syncing to Google Sheets:", e$message, "\n")
    return(FALSE)
  })
}

#' Sync Google Sheets data to local SQLite
sync_from_sheets <- function() {
  if (!SHEETS_CONFIG$authenticated) {
    cat("Not authenticated with Google Sheets - skipping sync\n")
    return(FALSE)
  }
  
  tryCatch({
    # Read from Google Sheets
    stretches <- read_from_sheets("stretches")
    daily_stats <- read_from_sheets("daily_stats")
    stretch_history <- read_from_sheets("stretch_history")
    
    if (!is.null(stretches) && nrow(stretches) > 0) {
      # Update local database
      con <- get_db_connection()
      
      # Clear and repopulate stretches
      dbExecute(con, "DELETE FROM stretches")
      for (i in 1:nrow(stretches)) {
        dbExecute(con, "
          INSERT INTO stretches (id, name, priority, category, description, enabled)
          VALUES (?, ?, ?, ?, ?, ?)
        ", params = list(
          stretches$id[i],
          stretches$name[i],
          stretches$priority[i],
          stretches$category[i],
          stretches$description[i],
          stretches$enabled[i]
        ))
      }
      
      dbDisconnect(con)
      
      # Save other data as RDS files
      if (!is.null(daily_stats) && nrow(daily_stats) > 0) {
        saveRDS(daily_stats, "data/daily_stats.rds")
      }
      
      if (!is.null(stretch_history) && nrow(stretch_history) > 0) {
        saveRDS(stretch_history, "data/stretch_history.rds")
      }
      
      cat("Successfully synced data from Google Sheets\n")
      return(TRUE)
    }
    
    return(FALSE)
  }, error = function(e) {
    cat("Error syncing from Google Sheets:", e$message, "\n")
    return(FALSE)
  })
}

#' Check if Google Sheets integration is available and working
is_sheets_available <- function() {
  return(SHEETS_CONFIG$authenticated && !is.null(SHEETS_CONFIG$main_sheet_id))
}

#' Get the main spreadsheet URL for sharing
get_spreadsheet_url <- function() {
  if (!is.null(SHEETS_CONFIG$main_sheet_id)) {
    return(paste0("https://docs.google.com/spreadsheets/d/", SHEETS_CONFIG$main_sheet_id))
  }
  return(NULL)
}