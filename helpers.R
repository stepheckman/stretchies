# Helper Functions for Stretch Tracker App
# Contains algorithms, statistics, and chart generation functions

# Load required database libraries (for compatibility)
library(DBI)
library(RSQLite)

# Check daily stretch limits for a specific stretch
check_daily_stretch_limit <- function(stretch_id, stretch_history) {
  today <- Sys.Date()
  
  # Get today's actions for this specific stretch
  today_actions <- stretch_history[
    stretch_history$stretch_id == stretch_id &
    stretch_history$date == today,
  ]
  
  if (nrow(today_actions) == 0) {
    return(list(can_deliver = TRUE, reason = "not_done_today"))
  }
  
  # Count completed and total actions for this stretch today
  completed_today <- sum(today_actions$action == "completed")
  total_today <- nrow(today_actions)
  
  # If completed once today, don't deliver again
  if (completed_today >= 1) {
    return(list(can_deliver = FALSE, reason = "completed_once_today"))
  }
  
  # If not completed but attempted twice, don't deliver again
  if (completed_today == 0 && total_today >= 2) {
    return(list(can_deliver = FALSE, reason = "attempted_twice_not_completed"))
  }
  
  return(list(can_deliver = TRUE, reason = "within_limits"))
}

# Get available stretches for today based on daily limits
get_available_stretches_today <- function(stretches, stretch_history) {
  available_stretches <- data.frame()
  
  for (i in 1:nrow(stretches)) {
    stretch_id <- stretches$id[i]
    limit_check <- check_daily_stretch_limit(stretch_id, stretch_history)
    
    if (limit_check$can_deliver) {
      available_stretches <- rbind(available_stretches, stretches[i, ])
    }
  }
  
  return(available_stretches)
}

# Smart stretch selection algorithm
select_next_stretch <- function() {
  stretches <- load_stretches_data()
  stretch_history <- load_stretch_history()
  user_prefs <- load_user_preferences()
  
  # Add enabled field if it doesn't exist (for backward compatibility)
  if (!"enabled" %in% names(stretches)) {
    stretches$enabled <- rep(TRUE, nrow(stretches)) # Initialize all to TRUE
  }
  
  # Filter to only enabled stretches, handling NA values in 'enabled' column
  enabled_stretches <- stretches[!is.na(stretches$enabled) & stretches$enabled == TRUE, ]

  if (nrow(enabled_stretches) == 0) {
    # If no stretches are enabled, return NULL or a default message
    return(list(
      id = 0,
      name = "No stretches available",
      description = "Please add some stretches in the Settings tab!",
      priority = "low",
      category = "general",
      is_supportive_message = TRUE
    ))
  }
  
  # Filter stretches based on daily limits
  available_today <- get_available_stretches_today(enabled_stretches, stretch_history)
  
  if (nrow(available_today) == 0) {
    # No stretches available today due to daily limits
    return(get_daily_limit_message())
  }
  
  # Calculate weights for available stretches only
  weights <- calculate_stretch_weights(available_today, stretch_history, user_prefs)
  
  # Select stretch based on weighted probability
  selected_id <- sample(available_today$id, 1, prob = weights)
  selected_stretch <- available_today[available_today$id == selected_id, ]
  
  return(selected_stretch)
}

# Get supportive message when daily limits are reached
get_daily_limit_message <- function() {
  supportive_messages <- c(
    "🌟 Amazing work today! You've reached your daily stretch goals. Your body is grateful for the care you've shown it!",
    "🎉 Fantastic job! You've completed your stretching for today. Take a moment to appreciate how good your body feels!",
    "💪 Well done! You've given your body the attention it deserves today. Rest and recovery are just as important!",
    "🧘‍♀️ Excellent dedication! You've honored your commitment to wellness today. Your future self will thank you!",
    "✨ Outstanding! You've completed your daily stretch routine. Enjoy the improved flexibility and mobility!",
    "🏆 Incredible consistency! You've reached today's stretch limit. Your body is stronger and more flexible because of your efforts!",
    "🌈 Beautiful work! You've given your muscles the love they needed today. Tomorrow brings new opportunities to stretch and grow!",
    "💚 Wonderful job! You've completed your stretching goals for today. Your dedication to self-care is inspiring!"
  )
  
  return(list(
    id = -1,
    name = "Daily Goals Achieved! 🎯",
    description = sample(supportive_messages, 1),
    priority = "high",
    category = "supportive",
    is_supportive_message = TRUE
  ))
}

# Calculate weights for stretch selection
calculate_stretch_weights <- function(stretches, stretch_history, user_prefs) {
  print(head(stretches))
  print(head(stretch_history))
  print(user_prefs)

  weights <- numeric(nrow(stretches))
  
  for (i in 1:nrow(stretches)) {
    stretch_id <- stretches$id[i]
    priority <- stretches$priority[i]
    cat("Processing stretch ID:", stretch_id, "Priority:", priority, "\n")
    
    # Base weight from priority
    cat("Priority for stretch ID", stretch_id, ": '", priority, "'\n")
    cat("is.na(priority):", is.na(priority), "\n")
    cat("!(priority %in% c('high', 'low')):", !(priority %in% c("high", "low")), "\n")
    
    if (is.na(priority) || !(priority %in% c("high", "low"))) {
      cat("Warning: Invalid or missing priority for stretch ID:", stretch_id, ". Defaulting to low priority.\n")
      base_weight <- user_prefs$low_priority_weight
    } else if (priority == "high") {
      base_weight <- user_prefs$high_priority_weight
    } else { # priority == "low"
      base_weight <- user_prefs$low_priority_weight
    }
    
    # Get stretch history for this specific stretch
    stretch_actions <- stretch_history[stretch_history$stretch_id == stretch_id, ]
    
    if (nrow(stretch_actions) == 0) {
      # Never done before - give bonus
      recency_weight <- user_prefs$never_done_bonus
    } else {
      # Calculate recency weight
      last_completed <- stretch_actions[stretch_actions$action == "completed", ]
      if (nrow(last_completed) > 0) {
        days_since_last <- as.numeric(Sys.Date() - max(last_completed$date))
        recency_weight <- min(days_since_last * user_prefs$recency_weight, 10) # Cap at 10
        cat("Calculated recency_weight (completed):", recency_weight, "\n")
      } else {
        # Never completed, only skipped
        recency_weight <- user_prefs$never_done_bonus * 0.5
        cat("Calculated recency_weight (never completed, only skipped):", recency_weight, "\n")
      }
    }
    
    # Combine weights
    weights[i] <- base_weight * (1 + recency_weight)
    cat("Combined weight for stretch ID", stretch_id, ":", weights[i], "\n")
  }
  
  cat("Raw weights before normalization:\n")
  print(weights)
  cat("Sum of raw weights:", sum(weights), "\n")
  
  # Normalize weights
  # Add a check to prevent division by zero or NA sum
  if (sum(weights) == 0 || is.na(sum(weights))) {
    weights <- rep(1 / nrow(stretches), nrow(stretches)) # Assign equal weights if sum is problematic
  } else {
    weights <- weights / sum(weights)
  }
  
  return(weights)
}

# Statistics functions
get_today_completed_count <- function(daily_stats) {
  if (is.null(daily_stats) || nrow(daily_stats) == 0) return(0)
  
  today <- Sys.Date()
  today_data <- daily_stats[daily_stats$date == today, ]
  
  if (nrow(today_data) == 0) {
    return(0)
  } else {
    return(today_data$completed_count[1])
  }
}

get_current_streak <- function(daily_stats) {
  if (is.null(daily_stats) || nrow(daily_stats) == 0) return(0)
  
  # Sort by date descending
  daily_stats <- daily_stats[order(daily_stats$date, decreasing = TRUE), ]
  
  streak <- 0
  for (i in 1:nrow(daily_stats)) {
    if (daily_stats$completed_count[i] > 0) {
      streak <- streak + 1
    } else {
      break
    }
  }
  
  return(streak)
}

get_total_stretches_completed <- function(stretch_history) {
  if (is.null(stretch_history) || nrow(stretch_history) == 0) return(0)
  
  completed <- stretch_history[stretch_history$action == "completed", ]
  return(nrow(completed))
}

get_total_active_days <- function(daily_stats) {
  if (is.null(daily_stats) || nrow(daily_stats) == 0) return(0)
  
  active_days <- daily_stats[daily_stats$completed_count > 0, ]
  return(nrow(active_days))
}

get_average_daily_stretches <- function(daily_stats) {
  if (is.null(daily_stats) || nrow(daily_stats) == 0) return(0)
  
  active_days <- daily_stats[daily_stats$completed_count > 0, ]
  if (nrow(active_days) == 0) return(0)
  
  return(mean(active_days$completed_count))
}

get_favorite_stretch <- function(stretch_history) {
  if (is.null(stretch_history) || nrow(stretch_history) == 0) return("None yet")
  
  completed <- stretch_history[stretch_history$action == "completed", ]
  if (nrow(completed) == 0) return("None yet")
  
  stretch_counts <- table(completed$stretch_name)
  favorite <- names(stretch_counts)[which.max(stretch_counts)]
  
  return(favorite)
}

# Motivational messages
get_motivational_message <- function(daily_stats) {
  today_count <- get_today_completed_count(daily_stats)
  streak <- get_current_streak(daily_stats)
  
  if (today_count == 0) {
    messages <- c(
      "Ready to start your stretching journey today? 🌟",
      "Your body is waiting for some love and movement! 💪",
      "Every stretch counts - let's begin! 🎯",
      "Time to show your muscles some care! 🤗"
    )
  } else if (today_count < 3) {
    messages <- c(
      paste("Great start! You've done", today_count, "stretch(es) today! 🎉"),
      "You're building momentum - keep it up! 🚀",
      "Your body is thanking you already! 💚",
      "Consistency is key - you're doing amazing! ⭐"
    )
  } else if (today_count < 5) {
    messages <- c(
      paste("Fantastic! You've completed", today_count, "stretches today! 🏆"),
      "You're on fire today! Your flexibility is improving! 🔥",
      "Look at you go! Your dedication is inspiring! 💫",
      "Your future self will thank you for this! 🙏"
    )
  } else {
    messages <- c(
      paste("INCREDIBLE! You've done", today_count, "stretches today! 🎊"),
      "You're a stretching superstar! 🌟",
      "Your commitment to wellness is outstanding! 👑",
      "You've exceeded expectations - amazing work! 🎯"
    )
  }
  
  if (streak > 1) {
    streak_messages <- c(
      paste("Plus you're on a", streak, "day streak! 🔥"),
      paste("Your", streak, "day streak is impressive! 💪"),
      paste("Day", streak, "of your amazing streak! 🚀")
    )
    messages <- c(messages, streak_messages)
  }
  
  return(sample(messages, 1))
}

# Chart creation functions
create_daily_progress_chart <- function(daily_stats) {
  if (is.null(daily_stats) || nrow(daily_stats) == 0) {
    # Return empty chart
    p <- plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = "No data yet - start stretching!",
                      textfont = list(size = 16, color = '#abb2bf')) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    plot_bgcolor = '#3a3f4b',
                    paper_bgcolor = '#3a3f4b')
    return(p)
  }
  
  # Ensure we have at least 7 days of data for the chart
  end_date <- Sys.Date()
  start_date <- end_date - 6
  
  date_range <- seq(start_date, end_date, by = "day")
  chart_data <- data.frame(
    date = date_range,
    completed = 0,
    stringsAsFactors = FALSE
  )
  
  # Fill in actual data
  for (i in 1:nrow(chart_data)) {
    day_data <- daily_stats[daily_stats$date == chart_data$date[i], ]
    if (nrow(day_data) > 0) {
      chart_data$completed[i] <- day_data$completed_count[1]
    }
  }
  
  p <- plotly::plot_ly(chart_data, x = ~date, y = ~completed, type = 'scatter', mode = 'lines+markers',
                      line = list(color = '#61afef', width = 3),
                      marker = list(color = '#98c379', size = 8)) %>%
    plotly::layout(title = "Daily Stretch Progress",
                  xaxis = list(title = "Date", color = '#abb2bf'),
                  yaxis = list(title = "Stretches Completed", color = '#abb2bf'),
                  hovermode = 'closest',
                  plot_bgcolor = '#3a3f4b',
                  paper_bgcolor = '#3a3f4b',
                  font = list(color = '#abb2bf'))
  
  return(p)
}

create_stretch_frequency_chart <- function(stretch_history) {
  if (is.null(stretch_history) || nrow(stretch_history) == 0) {
    p <- plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = "No stretch data yet!",
                      textfont = list(size = 16, color = '#abb2bf')) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    plot_bgcolor = '#3a3f4b',
                    paper_bgcolor = '#3a3f4b')
    return(p)
  }
  
  completed <- stretch_history[stretch_history$action == "completed", ]
  if (nrow(completed) == 0) {
    p <- plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = "Complete some stretches to see frequency!",
                      textfont = list(size = 16, color = '#abb2bf')) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    plot_bgcolor = '#3a3f4b',
                    paper_bgcolor = '#3a3f4b')
    return(p)
  }
  
  stretch_counts <- as.data.frame(table(completed$stretch_name))
  names(stretch_counts) <- c("stretch", "count")
  stretch_counts <- stretch_counts[order(stretch_counts$count, decreasing = TRUE), ]
  
  # Take top 10 for readability
  if (nrow(stretch_counts) > 10) {
    stretch_counts <- stretch_counts[1:10, ]
  }
  
  p <- plotly::plot_ly(stretch_counts, x = ~reorder(stretch, count), y = ~count,
                      type = 'bar', marker = list(color = '#56b6c2')) %>%
    plotly::layout(title = "Most Frequently Done Stretches",
                  xaxis = list(title = "Stretch", color = '#abb2bf'),
                  yaxis = list(title = "Times Completed", color = '#abb2bf'),
                  plot_bgcolor = '#3a3f4b',
                  paper_bgcolor = '#3a3f4b',
                  font = list(color = '#abb2bf')) %>%
    plotly::config(displayModeBar = FALSE)
  
  return(p)
}

create_weekly_trends_chart <- function(daily_stats) {
  if (is.null(daily_stats) || nrow(daily_stats) == 0) {
    p <- plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = "No weekly data yet!",
                      textfont = list(size = 16, color = '#abb2bf')) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    plot_bgcolor = '#3a3f4b',
                    paper_bgcolor = '#3a3f4b')
    return(p)
  }
  
  # Create weekly aggregation
  daily_stats$week <- format(daily_stats$date, "%Y-W%U")
  weekly_stats <- aggregate(completed_count ~ week, data = daily_stats, sum)
  
  if (nrow(weekly_stats) == 0) {
    p <- plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = "Complete stretches to see trends!",
                      textfont = list(size = 16, color = '#abb2bf')) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    plot_bgcolor = '#3a3f4b',
                    paper_bgcolor = '#3a3f4b')
    return(p)
  }
  
  p <- plotly::plot_ly(weekly_stats, x = ~week, y = ~completed_count,
                      type = 'bar', marker = list(color = '#e06c75')) %>%
    plotly::layout(title = "Weekly Stretch Totals",
                  xaxis = list(title = "Week", color = '#abb2bf'),
                  yaxis = list(title = "Total Stretches", color = '#abb2bf'),
                  plot_bgcolor = '#3a3f4b',
                  paper_bgcolor = '#3a3f4b',
                  font = list(color = '#abb2bf')) %>%
    plotly::config(displayModeBar = FALSE)
  
  return(p)
}

# Create detailed statistics table
create_detailed_stats_table <- function(stretch_history) {
  if (is.null(stretch_history) || nrow(stretch_history) == 0) {
    return(data.frame(
      Stretch = "No data yet",
      Completed = 0,
      Skipped = 0,
      Total = 0,
      "Success Rate" = "0%",
      "Last Done" = "Never",
      stringsAsFactors = FALSE
    ))
  }
  
  stretches <- load_stretches_data()
  
  stats_list <- list()
  
  for (i in 1:nrow(stretches)) {
    stretch_id <- stretches$id[i]
    stretch_name <- stretches$name[i]
    
    stretch_actions <- stretch_history[stretch_history$stretch_id == stretch_id, ]
    
    completed_count <- nrow(stretch_actions[stretch_actions$action == "completed", ])
    skipped_count <- nrow(stretch_actions[stretch_actions$action == "skipped", ])
    total_count <- completed_count + skipped_count
    
    if (total_count > 0) {
      success_rate <- paste0(round((completed_count / total_count) * 100, 1), "%")
    } else {
      success_rate <- "0%"
    }
    
    last_completed <- stretch_actions[stretch_actions$action == "completed", ]
    if (nrow(last_completed) > 0) {
      last_done <- format(max(last_completed$date), "%Y-%m-%d")
    } else {
      last_done <- "Never"
    }
    
    stats_list[[i]] <- data.frame(
      Stretch = stretch_name,
      Completed = completed_count,
      Skipped = skipped_count,
      Total = total_count,
      "Success Rate" = success_rate,
      "Last Done" = last_done,
      stringsAsFactors = FALSE
    )
  }
  
  detailed_stats <- do.call(rbind, stats_list)
  detailed_stats <- detailed_stats[order(detailed_stats$Completed, decreasing = TRUE), ]
  
  return(detailed_stats)
}