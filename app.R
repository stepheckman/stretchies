# Stretch Tracker Shiny App
# A fun and engaging app to track your daily stretches

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DBI)
library(RSQLite)
library(googlesheets4)
library(googledrive)
library(httr)
library(jsonlite)

# Source helper functions
source("helpers.R")
source("data_setup.R")
source("google_sheets_helpers.R")

# Initialize data
initialize_data()

# Initialize Google Sheets (if credentials are available)
google_auth_success <- initialize_google_auth()
if (google_auth_success) {
  setup_google_sheets()
  cat("Google Sheets integration enabled\n")
} else {
  cat("Running in local mode - Google Sheets integration disabled\n")
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "🧘‍♀️ Stretch Tracker", titleWidth = 250),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("🏠 Dashboard", tabName = "dashboard", icon = icon("home")),
      menuItem("📊 Progress", tabName = "progress", icon = icon("chart-line")),
      menuItem("📈 Statistics", tabName = "stats", icon = icon("chart-bar")),
      menuItem("⚙️ Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    # Initialize shinyjs
    useShinyjs(),
    
    # Custom CSS for dark theme styling
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #282c34;
          color: #abb2bf;
        }
        .main-header .navbar {
          background-color: #21252b !important;
        }
        .main-header .logo {
          background-color: #21252b !important;
        }
        .skin-blue .main-sidebar {
          background-color: #21252b;
        }
        .stretch-card {
          background-color: #3a3f4b;
          color: #abb2bf;
          border-radius: 15px;
          padding: 20px;
          margin: 10px 0;
          text-align: center;
          box-shadow: 0 4px 15px rgba(0,0,0,0.3);
          border: 1px solid #4a5568;
        }
        .stretch-name {
          font-size: 24px;
          font-weight: bold;
          margin-bottom: 10px;
          color: #ffffff;
        }
        .stretch-description {
          font-size: 16px;
          opacity: 0.9;
          margin-bottom: 20px;
          color: #abb2bf;
        }
        .action-buttons {
          margin-top: 20px;
        }
        .btn-stretch {
          background-color: #61afef;
          border: none;
          color: white;
          font-size: 18px;
          padding: 15px 30px;
          border-radius: 25px;
          margin: 5px;
          transition: all 0.3s ease;
        }
        .btn-stretch:hover {
          background-color: #4a9eff;
          transform: translateY(-2px);
          box-shadow: 0 5px 15px rgba(0,0,0,0.4);
        }
        .btn-done {
          background-color: #98c379;
        }
        .btn-done:hover {
          background-color: #7fb069;
        }
        .btn-skip {
          background-color: #e5c07b;
          color: #282c34;
        }
        .btn-skip:hover {
          background-color: #d4af37;
        }
        .stats-box {
          background-color: #3a3f4b;
          color: #abb2bf;
          border-radius: 10px;
          padding: 15px;
          margin: 10px 0;
          box-shadow: 0 2px 10px rgba(0,0,0,0.3);
          border: 1px solid #4a5568;
        }
        .motivational-message {
          background-color: #3a3f4b;
          border-radius: 10px;
          padding: 15px;
          margin: 15px 0;
          text-align: center;
          font-size: 16px;
          font-weight: 500;
          color: #abb2bf;
          border: 1px solid #4a5568;
        }
        .box {
          background-color: #3a3f4b;
          color: #abb2bf;
          border: 1px solid #4a5568;
        }
        .box-header {
          background-color: #21252b;
          color: #abb2bf;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #61afef;
        }
        .sidebar-menu > li > a {
          color: #abb2bf;
        }
        .sidebar-menu > li.active > a {
          background-color: #3a3f4b;
          color: #61afef;
        }
        /* DataTables styling for better readability */
        .dataTables_wrapper {
          color: #abb2bf !important;
        }
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate {
          color: #abb2bf !important;
        }
        .dataTables_wrapper .dataTables_length label,
        .dataTables_wrapper .dataTables_filter label {
          color: #abb2bf !important;
        }
        .dataTables_wrapper .dataTables_length select,
        .dataTables_wrapper .dataTables_filter input {
          background-color: #3a3f4b !important;
          color: #abb2bf !important;
          border: 1px solid #4a5568 !important;
        }
        .dataTables_wrapper table.dataTable thead th,
        .dataTables_wrapper table.dataTable thead td {
          background-color: #21252b !important;
          color: #abb2bf !important;
          border-bottom: 1px solid #4a5568 !important;
        }
        .dataTables_wrapper table.dataTable tbody tr {
          background-color: #3a3f4b !important;
          color: #abb2bf !important;
        }
        .dataTables_wrapper table.dataTable tbody tr:hover {
          background-color: #4a5568 !important;
        }
        .dataTables_wrapper table.dataTable tbody tr.selected {
          background-color: #61afef !important;
          color: #282c34 !important;
        }
        .dataTables_wrapper table.dataTable tbody td {
          border-top: 1px solid #4a5568 !important;
          color: #abb2bf !important;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          background-color: #3a3f4b !important;
          color: #abb2bf !important;
          border: 1px solid #4a5568 !important;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background-color: #4a5568 !important;
          color: #ffffff !important;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background-color: #61afef !important;
          color: #282c34 !important;
        }
        /* Form elements styling for modals */
        .modal .form-control {
          background-color: #3a3f4b !important;
          color: #abb2bf !important;
          border: 1px solid #4a5568 !important;
        }
        .modal .form-control:focus {
          background-color: #3a3f4b !important;
          color: #abb2bf !important;
          border-color: #61afef !important;
          box-shadow: 0 0 0 0.2rem rgba(97, 175, 239, 0.25) !important;
        }
        /* Modal styling */
        .modal-content {
          background-color: #3a3f4b !important;
          color: #abb2bf !important;
          border: 1px solid #4a5568 !important;
        }
        .modal-header {
          background-color: #21252b !important;
          color: #abb2bf !important;
          border-bottom: 1px solid #4a5568 !important;
        }
        .modal-title {
          color: #abb2bf !important;
        }
        /* Specific styling for app info text */
        pre {
          background-color: #21252b !important;
          color: #abb2bf !important;
          border: 1px solid #4a5568 !important;
        }
      "))
    ),
    
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
        fluidRow(
          column(width = 8,
            div(class = "stretch-card",
              uiOutput("stretch_display")
            ),
            
            div(class = "motivational-message",
              textOutput("motivational_message")
            )
          ),
          
          column(width = 4,
            div(class = "stats-box",
              h4("📊 Today's Progress"),
              valueBoxOutput("today_completed", width = 12)
            )
          )
        )
      ),
      
      # Progress tab
      tabItem(tabName = "progress",
        fluidRow(
          column(width = 6,
            box(title = "📅 Daily Progress", status = "primary", solidHeader = TRUE, width = NULL,
              withSpinner(plotlyOutput("daily_progress_plot"))
            )
          ),
          column(width = 6,
            box(title = "🎯 Stretch Frequency", status = "success", solidHeader = TRUE, width = NULL,
              withSpinner(plotlyOutput("stretch_frequency_plot"))
            )
          )
        ),
        fluidRow(
          column(width = 12,
            box(title = "📈 Weekly Trends", status = "info", solidHeader = TRUE, width = NULL,
              withSpinner(plotlyOutput("weekly_trends_plot"))
            )
          )
        )
      ),
      
      # Statistics tab
      tabItem(tabName = "stats",
        fluidRow(
          column(width = 4,
            valueBoxOutput("total_days_active", width = NULL)
          ),
          column(width = 4,
            valueBoxOutput("average_daily", width = NULL)
          ),
        ),
        fluidRow(
          column(width = 12,
            box(title = "📋 Detailed Statistics", status = "warning", solidHeader = TRUE, width = NULL,
              DT::dataTableOutput("detailed_stats")
            )
          )
        )
      ),
      
      # Settings tab
      tabItem(tabName = "settings",
        fluidRow(
          column(width = 8,
            box(title = "🎯 Stretch Management", status = "primary", solidHeader = TRUE, width = NULL,
              div(style = "display: flex; gap: 10px; flex-wrap: wrap; margin-bottom: 20px;",
                actionButton("add_stretch_btn", "➕ Add New Stretch",
                           class = "btn btn-success"),
                actionButton("edit_stretch_btn", "✏️ Edit Selected",
                           class = "btn btn-info"),
                actionButton("delete_stretch_btn", "🗑️ Delete Selected",
                           class = "btn btn-danger"),
                actionButton("reset_data", "🔄 Reset All Data",
                           class = "btn btn-warning",
                           onclick = "return confirm('Are you sure you want to reset all data?');")
              ),
              DT::dataTableOutput("stretch_table")
            )
          ),
          column(width = 4,
            box(title = "📊 App Statistics", status = "info", solidHeader = TRUE, width = NULL,
              verbatimTextOutput("app_info")
            ),
            box(title = "💾 Backup & Restore", status = "warning", solidHeader = TRUE, width = NULL,
              div(style = "margin-bottom: 15px;",
                p("Save your stretch modifications before redeploying:", style = "margin-bottom: 10px;"),
                downloadButton("download_stretches", "📥 Download Stretches CSV",
                             class = "btn btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                fileInput("upload_stretches", "📤 Upload Stretches CSV",
                         accept = ".csv", width = "100%"),
                div(id = "upload_status", style = "margin-top: 10px;")
              )
            ),
            box(title = "☁️ Google Sheets Sync", status = "info", solidHeader = TRUE, width = NULL,
              div(style = "margin-bottom: 15px;",
                conditionalPanel(
                  condition = "true", # We'll update this based on auth status
                  div(
                    p("Sync your data with Google Sheets for cloud backup:", style = "margin-bottom: 10px;"),
                    actionButton("sync_to_sheets", "☁️ Upload to Google Sheets",
                               class = "btn btn-success", style = "width: 100%; margin-bottom: 10px;"),
                    actionButton("sync_from_sheets", "📥 Download from Google Sheets",
                               class = "btn btn-info", style = "width: 100%; margin-bottom: 10px;"),
                    div(id = "sheets_status", style = "margin-top: 10px;",
                      textOutput("sheets_auth_status")
                    )
                  )
                )
              )
            )
          )
        ),
        
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    current_stretch = NULL,
    show_stretch = FALSE,
    daily_stats = NULL,
    stretch_history = NULL,
    form_mode = "none",  # none, add, edit
    editing_stretch_id = NULL
  )
  
  # Load data on startup
  observe({
    values$daily_stats <- load_daily_stats()
    values$stretch_history <- load_stretch_history()
  })
  
  # Initialize stretch display
  output$stretch_display <- renderUI({
    div(
      h3("Ready to stretch?"),
      p("Click the button below to get your next stretch!"),
      br(),
      actionButton("get_stretch", "🎯 Get My Stretch!",
                 class = "btn-stretch btn-lg",
                 style = "font-size: 20px; padding: 20px 40px;")
    )
  })
  
  # Get stretch button
  observeEvent(input$get_stretch, {
    cat("Get stretch button clicked\n")
    cat("Current time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    
    stretch <- select_next_stretch()
    values$current_stretch <- stretch
    values$show_stretch <- TRUE
    
    cat("After select_next_stretch - values$current_stretch (name):",
        ifelse(is.null(values$current_stretch$name), "NULL", values$current_stretch$name), "\n")
    cat("values$show_stretch:", values$show_stretch, "\n")
    
    # Update UI to show the stretch or supportive message
    output$stretch_display <- renderUI({
      cat("Rendering stretch_display UI.\n")
      cat("values$show_stretch in renderUI:", values$show_stretch, "\n")
      cat("is.null(values$current_stretch) in renderUI:", is.null(values$current_stretch), "\n")
      
      if (values$show_stretch && !is.null(values$current_stretch)) {
        cat("Rendering actual stretch display for:", values$current_stretch$name, "\n")
        
        # Check if this is a supportive message
        if (!is.null(values$current_stretch$is_supportive_message) && values$current_stretch$is_supportive_message) {
          # Display supportive message with different styling and no action buttons
          div(
            div(class = "stretch-name", values$current_stretch$name),
            div(class = "stretch-description", values$current_stretch$description),
            br(),
            div(style = "text-align: center; margin-top: 20px;",
              p("Come back tomorrow for more stretches! 🌅",
                style = "font-size: 16px; color: #98c379; font-weight: 500;")
            )
          )
        } else {
          # Display regular stretch with action buttons
          div(
            div(class = "stretch-name", values$current_stretch$name),
            div(class = "stretch-description", values$current_stretch$description),
            div(class = "action-buttons",
              actionButton("mark_done", "✅ Done!", class = "btn-done btn-lg"),
              actionButton("search_stretch", "🔍 Search Stretch", class = "btn-stretch"),
              br(), br(),
              actionButton("get_another", "🎯 Get Another Stretch", class = "btn-stretch")
            )
          )
        }
      } else {
        cat("Rendering initial/no stretch display.\n")
        div(
          h3("Ready to stretch?"),
          p("Click the button below to get your next stretch!"),
          br(),
          actionButton("get_stretch", "🎯 Get My Stretch!",
                     class = "btn-stretch btn-lg",
                     style = "font-size: 20px; padding: 20px 40px;")
        )
      }
    })
  })
  
  # Mark as done
  observeEvent(input$mark_done, {
    if (!is.null(values$current_stretch) &&
        (is.null(values$current_stretch$is_supportive_message) || !values$current_stretch$is_supportive_message)) {
      record_stretch_action(values$current_stretch$id, "completed")
      values$daily_stats <- load_daily_stats()
      values$stretch_history <- load_stretch_history()
      
      showNotification("Great job! Stretch completed! 🎉",
                      type = "message", duration = 3)

      # Automatically get the next stretch
      stretch <- select_next_stretch()
      values$current_stretch <- stretch
      values$show_stretch <- TRUE # Ensure the stretch is displayed

      # Update UI to show the next stretch or supportive message
      output$stretch_display <- renderUI({
        if (values$show_stretch && !is.null(values$current_stretch)) {
          # Check if this is a supportive message
          if (!is.null(values$current_stretch$is_supportive_message) && values$current_stretch$is_supportive_message) {
            # Display supportive message
            div(
              div(class = "stretch-name", values$current_stretch$name),
              div(class = "stretch-description", values$current_stretch$description),
              br(),
              div(style = "text-align: center; margin-top: 20px;",
                p("Come back tomorrow for more stretches! 🌅",
                  style = "font-size: 16px; color: #98c379; font-weight: 500;")
              )
            )
          } else {
            # Display regular stretch
            div(
              div(class = "stretch-name", values$current_stretch$name),
              div(class = "stretch-description", values$current_stretch$description),
              div(class = "action-buttons",
                actionButton("mark_done", "✅ Done!", class = "btn-done btn-lg"),
                actionButton("search_stretch", "🔍 Search Stretch", class = "btn-stretch"),
                br(), br(),
                actionButton("get_another", "🎯 Get Another Stretch", class = "btn-stretch")
              )
            )
          }
        } else {
          div(
            h3("Ready to stretch?"),
            p("Click the button below to get your next stretch!"),
            br(),
            actionButton("get_stretch", "🎯 Get My Stretch!",
                       class = "btn-stretch btn-lg",
                       style = "font-size: 20px; padding: 20px 40px;")
          )
        }
      })
    }
  })
  
  
  # Get another stretch
  observeEvent(input$get_another, {
    stretch <- select_next_stretch()
    values$current_stretch <- stretch
    
    # Update the current stretch display
    output$stretch_display <- renderUI({
      if (!is.null(values$current_stretch)) {
        # Check if this is a supportive message
        if (!is.null(values$current_stretch$is_supportive_message) && values$current_stretch$is_supportive_message) {
          # Display supportive message
          div(
            div(class = "stretch-name", values$current_stretch$name),
            div(class = "stretch-description", values$current_stretch$description),
            br(),
            div(style = "text-align: center; margin-top: 20px;",
              p("Come back tomorrow for more stretches! 🌅",
                style = "font-size: 16px; color: #98c379; font-weight: 500;")
            )
          )
        } else {
          # Display regular stretch
          div(
            div(class = "stretch-name", values$current_stretch$name),
            div(class = "stretch-description", values$current_stretch$description),
            div(class = "action-buttons",
              actionButton("mark_done", "✅ Done!", class = "btn-done btn-lg"),
              actionButton("search_stretch", "🔍 Search Stretch", class = "btn-stretch"),
              br(), br(),
              actionButton("get_another", "🎯 Get Another Stretch", class = "btn-stretch")
            )
          )
        }
      }
    }) # Closing parenthesis for renderUI
  }) # Closing parenthesis for observeEvent(input$get_another, ...)

  # Web search for current stretch
  observeEvent(input$search_stretch, {
    if (!is.null(values$current_stretch) &&
        (is.null(values$current_stretch$is_supportive_message) || !values$current_stretch$is_supportive_message)) {
      search_query <- URLencode(paste("stretch exercise", values$current_stretch$name), reserved = TRUE)
      search_url <- paste0("https://www.google.com/search?q=", search_query)
      shinyjs::runjs(paste0("window.open('", search_url, "', '_blank');"))
      showNotification(paste("Searching for:", values$current_stretch$name), type = "message", duration = 3)
    } else {
      showNotification("No stretch currently displayed to search for.", type = "warning", duration = 3)
    }
  })
  
  # Motivational message
  output$motivational_message <- renderText({
    get_motivational_message(values$daily_stats)
  })
  
  # Value boxes
  output$today_completed <- renderValueBox({
    today_count <- get_today_completed_count(values$daily_stats)
    valueBox(
      value = today_count,
      subtitle = "Completed Today",
      icon = icon("check-circle"),
      color = "light-blue"
    )
  })
  
  
  # Charts
  output$daily_progress_plot <- renderPlotly({
    create_daily_progress_chart(values$daily_stats)
  })
  
  output$stretch_frequency_plot <- renderPlotly({
    create_stretch_frequency_chart(values$stretch_history)
  })
  
  output$weekly_trends_plot <- renderPlotly({
    create_weekly_trends_chart(values$daily_stats)
  })
  
  # Statistics tab
  output$total_days_active <- renderValueBox({
    days <- get_total_active_days(values$daily_stats)
    valueBox(
      value = days,
      subtitle = "Days Active",
      icon = icon("calendar-check"),
      color = "light-blue"
    )
  })
  
  output$average_daily <- renderValueBox({
    avg <- get_average_daily_stretches(values$daily_stats)
    valueBox(
      value = round(avg, 1),
      subtitle = "Avg per Day",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  
  output$detailed_stats <- DT::renderDataTable({
    create_detailed_stats_table(values$stretch_history)
  }, options = list(pageLength = 10))
  
  # Settings tab
  output$stretch_table <- DT::renderDataTable({
    # React to the refresh trigger
    refresh_trigger <- stretch_table_refresh()
    cat("Rendering stretch table (refresh #", refresh_trigger, ")\n")
    
    stretches <- load_stretches_data()
    cat("Loaded stretches count:", nrow(stretches), "\n")
    
    # Add row selection and make it more user-friendly
    stretches$enabled <- ifelse(is.null(stretches$enabled), TRUE, stretches$enabled)
    stretches
  }, options = list(
    pageLength = 15,
    selection = 'single',
    scrollX = TRUE
  ), server = TRUE)
  
  output$app_info <- renderText({
    paste(
      "App Version: 1.0.0",
      paste("Total Stretches in Database:", nrow(load_stretches_data())),
      paste("Data Directory:", getwd()),
      paste("Last Updated:", Sys.time()),
      sep = "\n"
    )
  })
  
  # Stretch Management Event Handlers
  
  # Form state management
  
  
  # Add new stretch button
  observeEvent(input$add_stretch_btn, {
    cat("Add stretch button clicked\n")
    cat("Current time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    
    # Show modal directly
    showModal(modalDialog(
      title = "Add New Stretch",
      fluidRow(
        column(width = 6,
          textInput("stretch_name", "Stretch Name:",
                  placeholder = "Enter stretch name...")
        ),
        column(width = 6,
          selectInput("stretch_priority", "Priority:",
                    choices = list("High" = "high", "Low" = "low"),
                    selected = "low")
        )
      ),
      fluidRow(
        column(width = 6,
          selectInput("stretch_category", "Category:",
                    choices = list(
                      "Hips" = "hips",
                      "Core" = "core",
                      "Feet & Ankles" = "feet_ankles",
                      "Spine & Shoulders" = "spine_shoulders",
                      "Functional" = "functional",
                      "Mobility" = "mobility",
                      "Flexibility" = "flexibility",
                      "General" = "general"
                    ),
                    selected = "general")
        ),
        column(width = 6,
          div(style = "margin-top: 25px;",
            checkboxInput("stretch_enabled", "Enabled", value = TRUE)
          )
        )
      ),
      textAreaInput("stretch_description", "Description:",
                  placeholder = "Enter a detailed description of the stretch...",
                  rows = 4),
      footer = tagList(
        actionButton("save_stretch", "💾 Save Stretch", class = "btn btn-primary"),
        modalButton("Cancel")
      ),
      size = "l"
    ))
    
    # Set form mode
    values$form_mode <- "add"
  })
  
  # Edit stretch button
  observeEvent(input$edit_stretch_btn, {
    selected_row <- input$stretch_table_rows_selected
    if (length(selected_row) == 0) {
      showNotification("Please select a stretch to edit.", type = "warning", duration = 3)
      return()
    }
    
    stretches <- load_stretches_data()
    selected_stretch <- stretches[selected_row, ]
    
    # Show modal with populated data
    showModal(modalDialog(
      title = paste("Edit Stretch:", selected_stretch$name),
      fluidRow(
        column(width = 6,
          textInput("stretch_name", "Stretch Name:",
                  value = selected_stretch$name)
        ),
        column(width = 6,
          selectInput("stretch_priority", "Priority:",
                    choices = list("High" = "high", "Low" = "low"),
                    selected = selected_stretch$priority)
        )
      ),
      fluidRow(
        column(width = 6,
          selectInput("stretch_category", "Category:",
                    choices = list(
                      "Hips" = "hips",
                      "Core" = "core",
                      "Feet & Ankles" = "feet_ankles",
                      "Spine & Shoulders" = "spine_shoulders",
                      "Functional" = "functional",
                      "Mobility" = "mobility",
                      "Flexibility" = "flexibility",
                      "General" = "general"
                    ),
                    selected = selected_stretch$category)
        ),
        column(width = 6,
          div(style = "margin-top: 25px;",
            checkboxInput("stretch_enabled", "Enabled",
                        value = ifelse(is.null(selected_stretch$enabled), TRUE, selected_stretch$enabled))
          )
        )
      ),
      textAreaInput("stretch_description", "Description:",
                  value = selected_stretch$description,
                  rows = 4),
      footer = tagList(
        actionButton("save_stretch", "💾 Update Stretch", class = "btn btn-primary"),
        modalButton("Cancel")
      ),
      size = "l"
    ))
    
    # Set form mode and store ID
    values$form_mode <- "edit"
    values$editing_stretch_id <- selected_stretch$id
  })
  
  
  # Delete stretch button
  observeEvent(input$delete_stretch_btn, {
    selected_row <- input$stretch_table_rows_selected
    if (length(selected_row) == 0) {
      showNotification("Please select a stretch to delete.", type = "warning", duration = 3)
      return()
    }
    
    stretches <- load_stretches_data()
    selected_stretch <- stretches[selected_row, ]
    
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Are you sure you want to delete the stretch:", selected_stretch$name, "?"),
      br(), br(),
      "This action cannot be undone.",
      footer = tagList(
        actionButton("confirm_delete", "🗑️ Delete", class = "btn btn-danger"),
        modalButton("Cancel")
      )
    ))
    
    values$deleting_stretch_id <- selected_stretch$id
  })
  
  # Create a reactive value to track when to refresh the stretch table
  stretch_table_refresh <- reactiveVal(0)
  
  # Save stretch (handles both add and edit)
  observeEvent(input$save_stretch, {
    cat("Save stretch button clicked\n")
    cat("Current time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Form mode:", values$form_mode, "\n")
    cat("Form data - Name:", input$stretch_name,
        "Priority:", input$stretch_priority,
        "Category:", input$stretch_category,
        "Enabled:", input$stretch_enabled, "\n")
    
    # Validate input using the validation function
    validation <- validate_stretch_data(
      input$stretch_name,
      input$stretch_priority,
      input$stretch_category,
      input$stretch_description
    )
    
    cat("Validation result:", validation$valid, "\n")
    if (!validation$valid) {
      cat("Validation errors:", paste(validation$errors, collapse = "; "), "\n")
      showNotification(paste("Validation errors:", paste(validation$errors, collapse = "; ")),
                      type = "error", duration = 5)
      return()
    }
    
    if (values$form_mode == "add") {
      # Add the new stretch
      result <- add_new_stretch(
        name = trimws(input$stretch_name),
        priority = input$stretch_priority,
        category = input$stretch_category,
        description = trimws(input$stretch_description),
        enabled = input$stretch_enabled
      )
      
      if (result$success) {
        showNotification("Stretch added successfully! 🎉", type = "message", duration = 3)
        removeModal()  # Close the modal
        # Trigger a refresh of the stretch table
        stretch_table_refresh(stretch_table_refresh() + 1)
        cat("Triggered stretch table refresh after adding\n")
      } else {
        showNotification(paste("Error:", result$message), type = "error", duration = 5)
      }
    } else if (values$form_mode == "edit") {
      # Update the stretch
      result <- update_stretch(
        id = values$editing_stretch_id,
        name = trimws(input$stretch_name),
        priority = input$stretch_priority,
        category = input$stretch_category,
        description = trimws(input$stretch_description),
        enabled = input$stretch_enabled
      )
      
      if (result$success) {
        showNotification("Stretch updated successfully! ✅", type = "message", duration = 3)
        removeModal()  # Close the modal
        # Trigger a refresh of the stretch table
        stretch_table_refresh(stretch_table_refresh() + 1)
        cat("Triggered stretch table refresh after editing\n")
      } else {
        showNotification(paste("Error:", result$message), type = "error", duration = 5)
      }
    }
  })
  
  # Confirm delete stretch
  observeEvent(input$confirm_delete, {
    cat("Confirm delete button clicked for stretch ID:", values$deleting_stretch_id, "\n")
    result <- delete_stretch(values$deleting_stretch_id)
    
    if (result$success) {
      showNotification("Stretch deleted successfully.", type = "message", duration = 3)
      removeModal()
      # Trigger a refresh of the stretch table
      stretch_table_refresh(stretch_table_refresh() + 1)
      cat("Triggered stretch table refresh after deletion\n")
    } else {
      showNotification(paste("Error:", result$message), type = "error", duration = 5)
    }
  })
  
  # Reset data
  observeEvent(input$reset_data, {
    reset_all_data()
    values$daily_stats <- load_daily_stats()
    values$stretch_history <- load_stretch_history()
    showNotification("All data has been reset!", type = "warning", duration = 5)
  })
  
  # Download stretches as CSV
  output$download_stretches <- downloadHandler(
    filename = function() {
      paste0("stretches_backup_", Sys.Date(), ".csv")
    },
    content = function(file) {
      stretches <- load_stretches_data()
      write.csv(stretches, file, row.names = FALSE)
    }
  )
  
  # Upload stretches from CSV
  observeEvent(input$upload_stretches, {
    if (is.null(input$upload_stretches)) return()
    
    tryCatch({
      # Read the uploaded CSV file
      uploaded_stretches <- read.csv(input$upload_stretches$datapath, stringsAsFactors = FALSE)
      
      # Validate required columns
      required_cols <- c("id", "name", "priority", "category", "description", "enabled")
      if (!all(required_cols %in% names(uploaded_stretches))) {
        showNotification("❌ Invalid CSV format. Missing required columns.", type = "error", duration = 5)
        return()
      }
      
      # Clear existing stretches and insert uploaded ones
      con <- get_db_connection()
      dbExecute(con, "DELETE FROM stretches")
      
      for (i in 1:nrow(uploaded_stretches)) {
        dbExecute(con, "
          INSERT INTO stretches (id, name, priority, category, description, enabled)
          VALUES (?, ?, ?, ?, ?, ?)
        ", params = list(
          uploaded_stretches$id[i],
          uploaded_stretches$name[i],
          uploaded_stretches$priority[i],
          uploaded_stretches$category[i],
          uploaded_stretches$description[i],
          uploaded_stretches$enabled[i]
        ))
      }
      
      dbDisconnect(con)
      
      # Refresh the stretch table
      stretch_table_refresh(stretch_table_refresh() + 1)
      
      showNotification(paste("✅ Successfully restored", nrow(uploaded_stretches), "stretches!"),
                      type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("❌ Upload failed:", e$message), type = "error", duration = 5)
    })
  })
  
  # Google Sheets sync functionality
  output$sheets_auth_status <- renderText({
    if (is_sheets_available()) {
      paste("✅ Connected to Google Sheets")
    } else {
      "❌ Google Sheets not connected"
    }
  })
  
  # Sync to Google Sheets
  observeEvent(input$sync_to_sheets, {
    if (!is_sheets_available()) {
      showNotification("❌ Google Sheets not available. Please check authentication.",
                      type = "error", duration = 5)
      return()
    }
    
    tryCatch({
      success <- sync_to_sheets()
      if (success) {
        showNotification("✅ Successfully synced data to Google Sheets!",
                        type = "message", duration = 5)
        
        # Show the spreadsheet URL if available
        url <- get_spreadsheet_url()
        if (!is.null(url)) {
          showNotification(paste("📊 View your data:", url),
                          type = "message", duration = 10)
        }
      } else {
        showNotification("❌ Failed to sync to Google Sheets",
                        type = "error", duration = 5)
      }
    }, error = function(e) {
      showNotification(paste("❌ Sync error:", e$message),
                      type = "error", duration = 5)
    })
  })
  
  # Sync from Google Sheets
  observeEvent(input$sync_from_sheets, {
    if (!is_sheets_available()) {
      showNotification("❌ Google Sheets not available. Please check authentication.",
                      type = "error", duration = 5)
      return()
    }
    
    tryCatch({
      success <- sync_from_sheets()
      if (success) {
        # Refresh all reactive data
        values$daily_stats <- load_daily_stats()
        values$stretch_history <- load_stretch_history()
        stretch_table_refresh(stretch_table_refresh() + 1)
        
        showNotification("✅ Successfully synced data from Google Sheets!",
                        type = "message", duration = 5)
      } else {
        showNotification("❌ Failed to sync from Google Sheets",
                        type = "error", duration = 5)
      }
    }, error = function(e) {
      showNotification(paste("❌ Sync error:", e$message),
                      type = "error", duration = 5)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)