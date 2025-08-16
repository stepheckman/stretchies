# Helper Functions for Stretch Tracker App
# Contains algorithms, statistics, and chart generation functions

# Smart stretch selection algorithm
select_next_stretch <- function() {
  stretches <- load_stretches_data()
  stretch_history <- load_stretch_history()
  user_prefs <- load_user_preferences()
  
  # Calculate weights for each stretch
  weights <- calculate_stretch_weights(stretches, stretch_history, user_prefs)
  
  # Select stretch based on weighted probability
  selected_id <- sample(stretches$id, 1, prob = weights)
  selected_stretch <- stretches[stretches$id == selected_id, ]
  
  return(selected_stretch)
}

# Calculate weights for stretch selection
calculate_stretch_weights <- function(stretches, stretch_history, user_prefs) {
  weights <- numeric(nrow(stretches))
  
  for (i in 1:nrow(stretches)) {
    stretch_id <- stretches$id[i]
    priority <- stretches$priority[i]
    
    # Base weight from priority
    if (priority == "high") {
      base_weight <- user_prefs$high_priority_weight
    } else {
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
      } else {
        # Never completed, only skipped
        recency_weight <- user_prefs$never_done_bonus * 0.5
      }
    }
    
    # Combine weights
    weights[i] <- base_weight * (1 + recency_weight)
  }
  
  # Normalize weights
  weights <- weights / sum(weights)
  
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
                      textfont = list(size = 16)) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, showticklabels = FALSE))
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
                      line = list(color = '#667eea', width = 3),
                      marker = list(color = '#764ba2', size = 8)) %>%
    plotly::layout(title = "Daily Stretch Progress",
                  xaxis = list(title = "Date"),
                  yaxis = list(title = "Stretches Completed"),
                  hovermode = 'closest')
  
  return(p)
}

create_stretch_frequency_chart <- function(stretch_history) {
  if (is.null(stretch_history) || nrow(stretch_history) == 0) {
    p <- plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = "No stretch data yet!", 
                      textfont = list(size = 16)) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, showticklabels = FALSE))
    return(p)
  }
  
  completed <- stretch_history[stretch_history$action == "completed", ]
  if (nrow(completed) == 0) {
    p <- plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = "Complete some stretches to see frequency!", 
                      textfont = list(size = 16)) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, showticklabels = FALSE))
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
                      type = 'bar', marker = list(color = '#4ecdc4')) %>%
    plotly::layout(title = "Most Frequently Done Stretches",
                  xaxis = list(title = "Stretch"),
                  yaxis = list(title = "Times Completed")) %>%
    plotly::config(displayModeBar = FALSE)
  
  return(p)
}

create_weekly_trends_chart <- function(daily_stats) {
  if (is.null(daily_stats) || nrow(daily_stats) == 0) {
    p <- plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = "No weekly data yet!", 
                      textfont = list(size = 16)) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, showticklabels = FALSE))
    return(p)
  }
  
  # Create weekly aggregation
  daily_stats$week <- format(daily_stats$date, "%Y-W%U")
  weekly_stats <- aggregate(completed_count ~ week, data = daily_stats, sum)
  
  if (nrow(weekly_stats) == 0) {
    p <- plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = "Complete stretches to see trends!", 
                      textfont = list(size = 16)) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, showticklabels = FALSE))
    return(p)
  }
  
  p <- plotly::plot_ly(weekly_stats, x = ~week, y = ~completed_count, 
                      type = 'bar', marker = list(color = '#ff6b6b')) %>%
    plotly::layout(title = "Weekly Stretch Totals",
                  xaxis = list(title = "Week"),
                  yaxis = list(title = "Total Stretches")) %>%
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