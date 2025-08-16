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
              div(id = "stretch-display",
                h3("Ready to stretch?"),
                p("Click the button below to get your next stretch!"),
                br(),
                actionButton("get_stretch", "🎯 Get My Stretch!", 
                           class = "btn-stretch btn-lg", 
                           style = "font-size: 20px; padding: 20px 40px;")
              )
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
          column(width = 4,
            valueBoxOutput("favorite_stretch", width = NULL)
          )
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
          column(width = 6,
            box(title = "🎯 Stretch Management", status = "primary", solidHeader = TRUE, width = NULL,
              DT::dataTableOutput("stretch_table"),
              br(),
              actionButton("reset_data", "🔄 Reset All Data", 
                         class = "btn btn-warning",
                         onclick = "return confirm('Are you sure you want to reset all data?');")
            )
          ),
          column(width = 6,
            box(title = "📊 App Statistics", status = "info", solidHeader = TRUE, width = NULL,
              verbatimTextOutput("app_info")
            )
          )
        )
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
    stretch_history = NULL
  )
  
  # Load data on startup
  observe({
    values$daily_stats <- load_daily_stats()
    values$stretch_history <- load_stretch_history()
  })
  
  # Get stretch button
  observeEvent(input$get_stretch, {
    stretch <- select_next_stretch()
    values$current_stretch <- stretch
    values$show_stretch <- TRUE
    
    # Update UI to show the stretch
    output$stretch_display <- renderUI({
      if (values$show_stretch && !is.null(values$current_stretch)) {
        div(
          div(class = "stretch-name", values$current_stretch$name),
          div(class = "stretch-description", values$current_stretch$description),
          div(class = "action-buttons",
            actionButton("mark_done", "✅ Done!", class = "btn-done btn-lg"),
            actionButton("mark_skip", "⏭️ Skip", class = "btn-skip btn-lg"),
            br(), br(),
            actionButton("get_another", "🎯 Get Another Stretch", class = "btn-stretch")
          )
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
                      type = "success", duration = 3)
      
      # Reset display
      values$show_stretch <- FALSE
      output$stretch_display <- renderUI({
        div(
          h3("Awesome work! 🌟"),
          p("You completed a stretch! Keep up the great work!"),
          br(),
          actionButton("get_stretch", "🎯 Get My Next Stretch!", 
                     class = "btn-stretch btn-lg", 
                     style = "font-size: 20px; padding: 20px 40px;")
        )
      })
    }
  })
  
  # Mark as skip
  observeEvent(input$mark_skip, {
    if (!is.null(values$current_stretch)) {
      record_stretch_action(values$current_stretch$id, "skipped")
      values$daily_stats <- load_daily_stats()
      values$stretch_history <- load_stretch_history()
      
      showNotification("No worries! Try another stretch! 💪", 
                      type = "message", duration = 3)
      
      # Reset display
      values$show_stretch <- FALSE
      output$stretch_display <- renderUI({
        div(
          h3("That's okay! 😊"),
          p("Not every stretch is right for every moment. Let's find another one!"),
          br(),
          actionButton("get_stretch", "🎯 Get My Next Stretch!", 
                     class = "btn-stretch btn-lg", 
                     style = "font-size: 20px; padding: 20px 40px;")
        )
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
            actionButton("mark_skip", "⏭️ Skip", class = "btn-skip btn-lg"),
            br(), br(),
            actionButton("get_another", "🎯 Get Another Stretch", class = "btn-stretch")
          )
        )
      }
    })
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
  
  output$favorite_stretch <- renderValueBox({
    favorite <- get_favorite_stretch(values$stretch_history)
    valueBox(
      value = favorite,
      subtitle = "Favorite Stretch",
      icon = icon("heart"),
      color = "red"
    )
  })
  
  output$detailed_stats <- DT::renderDataTable({
    create_detailed_stats_table(values$stretch_history)
  }, options = list(pageLength = 10))
  
  # Settings tab
  output$stretch_table <- DT::renderDataTable({
    load_stretches_data()
  }, options = list(pageLength = 15), editable = TRUE)
  
  output$app_info <- renderText({
    paste(
      "App Version: 1.0.0",
      paste("Total Stretches in Database:", nrow(load_stretches_data())),
      paste("Data Directory:", getwd()),
      paste("Last Updated:", Sys.time()),
      sep = "\n"
    )
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