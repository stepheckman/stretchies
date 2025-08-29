# Stretch Tracker Shiny App
# A fun and engaging app to track your daily stretches

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DBI)
library(RSQLite)

# Source helper functions
source("helpers.R")
source("data_setup.R")

# Initialize data
initialize_data()

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
    # Custom CSS for fun styling
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f7ff;
        }
        .main-header .navbar {
          background-color: #667eea !important;
        }
        .main-header .logo {
          background-color: #667eea !important;
        }
        .skin-blue .main-sidebar {
          background-color: #764ba2;
        }
        .stretch-card {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-radius: 15px;
          padding: 20px;
          margin: 10px 0;
          text-align: center;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        .stretch-name {
          font-size: 24px;
          font-weight: bold;
          margin-bottom: 10px;
        }
        .stretch-description {
          font-size: 16px;
          opacity: 0.9;
          margin-bottom: 20px;
        }
        .action-buttons {
          margin-top: 20px;
        }
        .btn-stretch {
          background: linear-gradient(45deg, #ff6b6b, #ffa500);
          border: none;
          color: white;
          font-size: 18px;
          padding: 15px 30px;
          border-radius: 25px;
          margin: 5px;
          transition: all 0.3s ease;
        }
        .btn-stretch:hover {
          transform: translateY(-2px);
          box-shadow: 0 5px 15px rgba(0,0,0,0.2);
        }
        .btn-done {
          background: linear-gradient(45deg, #4ecdc4, #44a08d);
        }
        .btn-skip {
          background: linear-gradient(45deg, #ffeaa7, #fdcb6e);
        }
        .stats-box {
          background: white;
          border-radius: 10px;
          padding: 15px;
          margin: 10px 0;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .motivational-message {
          background: linear-gradient(135deg, #a8edea 0%, #fed6e3 100%);
          border-radius: 10px;
          padding: 15px;
          margin: 15px 0;
          text-align: center;
          font-size: 16px;
          font-weight: 500;
          color: #2d3436;
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
              valueBoxOutput("today_completed", width = 12),
              valueBoxOutput("current_streak", width = 12),
              valueBoxOutput("total_stretches", width = 12)
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
    
    # Update UI to show the stretch
    output$stretch_display <- renderUI({
      cat("Rendering stretch_display UI.\n")
      cat("values$show_stretch in renderUI:", values$show_stretch, "\n")
      cat("is.null(values$current_stretch) in renderUI:", is.null(values$current_stretch), "\n")
      
      if (values$show_stretch && !is.null(values$current_stretch)) {
        cat("Rendering actual stretch display for:", values$current_stretch$name, "\n")
        div(
          div(class = "stretch-name", values$current_stretch$name),
          div(class = "stretch-description", values$current_stretch$description),
          div(class = "action-buttons",
            actionButton("mark_done", "✅ Done!", class = "btn-done btn-lg"),
            actionButton("search_stretch", "🔍 Search Stretch", class = "btn-stretch"), # New button
            br(), br(),
            actionButton("get_another", "🎯 Get Another Stretch", class = "btn-stretch")
          )
        )
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
    if (!is.null(values$current_stretch)) {
      record_stretch_action(values$current_stretch$id, "completed")
      values$daily_stats <- load_daily_stats()
      values$stretch_history <- load_stretch_history()
      
      showNotification("Great job! Stretch completed! 🎉",
                      type = "message", duration = 3)

      # Automatically get the next stretch
      stretch <- select_next_stretch()
      values$current_stretch <- stretch
      values$show_stretch <- TRUE # Ensure the stretch is displayed

      # Update UI to show the next stretch directly
      output$stretch_display <- renderUI({
        if (values$show_stretch && !is.null(values$current_stretch)) {
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
        div(
          div(class = "stretch-name", values$current_stretch$name),
          div(class = "stretch-description", values$current_stretch$description),
          div(class = "action-buttons",
            actionButton("mark_done", "✅ Done!", class = "btn-done btn-lg"),
            actionButton("search_stretch", "🔍 Search Stretch", class = "btn-stretch"), # New button
            br(), br(),
            actionButton("get_another", "🎯 Get Another Stretch", class = "btn-stretch")
          )
        )
      }
    }) # Closing parenthesis for renderUI
  }) # Closing parenthesis for observeEvent(input$get_another, ...)

  # Web search for current stretch
  observeEvent(input$search_stretch, {
    if (!is.null(values$current_stretch)) {
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
      color = "green"
    )
  })
  
  output$current_streak <- renderValueBox({
    streak <- get_current_streak(values$daily_stats)
    valueBox(
      value = streak,
      subtitle = "Day Streak",
      icon = icon("fire"),
      color = "orange"
    )
  })
  
  output$total_stretches <- renderValueBox({
    total <- get_total_stretches_completed(values$stretch_history)
    valueBox(
      value = total,
      subtitle = "Total Completed",
      icon = icon("trophy"),
      color = "blue"
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
      color = "purple"
    )
  })
  
  output$average_daily <- renderValueBox({
    avg <- get_average_daily_stretches(values$daily_stats)
    valueBox(
      value = round(avg, 1),
      subtitle = "Avg per Day",
      icon = icon("chart-line"),
      color = "teal"
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
}

# Run the application
shinyApp(ui = ui, server = server)