# 🧘‍♀️ Stretch Tracker - Your Personal Stretching Companion

A fun and engaging R Shiny app to help you maintain a consistent stretching routine with smart stretch selection, progress tracking, and motivational features.

## ✨ Features

- **Smart Stretch Selection**: Weighted algorithm that prioritizes high-priority stretches while ensuring variety
- **Progress Tracking**: Track daily completions, streaks, and overall progress
- **Interactive Charts**: Visualize your progress with daily, weekly, and frequency charts
- **Motivational System**: Encouraging messages and achievement tracking
- **Custom Stretch Database**: Uses your personal list of stretches with priority levels
- **Fun UI**: Colorful, engaging interface with smooth animations

## 🚀 Quick Start

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Installation

1. **Clone or download this repository** to your local machine

2. **Install required packages** by running the installation script:
   ```r
   source("install_packages.R")
   ```
   
   Or install packages manually:
   ```r
   install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
                      "shinyWidgets", "shinycssloaders", "dplyr", 
                      "ggplot2", "lubridate"))
   ```

3. **Ensure your stretch list is ready**:
   - The app uses `list.csv` which should contain your stretches and priorities
   - Format: `Stretch,Priority` (where Priority is either "high" or "low")

4. **Run the app**:
   ```r
   shiny::runApp()
   ```
   
   Or open `app.R` in RStudio and click "Run App"

## 📁 File Structure

```
stretchies/
├── app.R                 # Main Shiny application
├── helpers.R             # Core functions and algorithms
├── data_setup.R          # Data initialization and management
├── install_packages.R    # Package installation script
├── list.csv             # Your stretch database
├── README.md            # This file
└── data/                # Created automatically for storing progress
    ├── stretches.rds
    ├── daily_stats.rds
    ├── stretch_history.rds
    └── user_preferences.rds
```

## 🎯 How It Works

### Smart Selection Algorithm
The app uses a weighted selection system that considers:
- **Priority Level**: High priority stretches get 3x weight vs low priority (1x weight)
- **Recency**: Stretches not done recently get higher weight
- **Never Done Bonus**: Stretches you've never completed get extra weight
- **Variety**: Ensures you don't get the same stretch repeatedly

### Progress Tracking
- **Daily Stats**: Tracks completed and skipped stretches per day
- **Streak Counting**: Maintains your consecutive day streak
- **Detailed History**: Records every stretch action with timestamps
- **Success Rates**: Calculates completion rates for each stretch

## 🎮 Using the App

### Dashboard Tab
- Click "Get My Stretch!" to receive a weighted-random stretch
- Mark stretches as "Done" or "Skip" 
- View today's progress and current streak
- Get motivational messages based on your progress

### Progress Tab
- **Daily Progress**: Line chart showing stretches completed over the last 7 days
- **Stretch Frequency**: Bar chart of your most frequently completed stretches
- **Weekly Trends**: Weekly totals to see long-term patterns

### Statistics Tab
- Detailed statistics including total active days, averages, and favorite stretches
- Comprehensive table showing completion rates for each stretch

### Settings Tab
- View and manage your stretch database
- Reset all data if needed
- View app information and statistics

## 🎨 Customization

### Adding New Stretches
Edit `list.csv` to add new stretches:
```csv
Stretch,Priority
Your New Stretch,high
Another Stretch,low
```

### Adjusting Algorithm Weights
Modify the weights in `data_setup.R` in the `user_preferences` section:
- `high_priority_weight`: Weight for high priority stretches (default: 3)
- `low_priority_weight`: Weight for low priority stretches (default: 1)
- `recency_weight`: How much recency affects selection (default: 2)
- `never_done_bonus`: Extra weight for never-completed stretches (default: 5)

### UI Customization
The app uses custom CSS in `app.R` for styling. You can modify colors, fonts, and animations by editing the CSS in the `tags$head()` section.

## 📊 Data Storage

All data is stored locally in RDS files in the `data/` directory:
- **stretches.rds**: Your stretch database
- **daily_stats.rds**: Daily completion statistics
- **stretch_history.rds**: Complete history of all stretch actions
- **user_preferences.rds**: Algorithm settings and preferences

## 🔧 Troubleshooting

### Package Installation Issues
If you encounter package installation errors:
1. Update R to the latest version
2. Try installing packages one by one
3. Check your internet connection
4. For Mac users, you may need Xcode command line tools

### App Won't Start
1. Ensure all required packages are installed
2. Check that `list.csv` exists and is properly formatted
3. Verify you're in the correct directory when running the app

### Data Issues
- If you want to reset all progress: use the "Reset All Data" button in Settings
- If data files become corrupted: delete the `data/` folder and restart the app

## 🎉 Tips for Success

1. **Start Small**: Begin with 2-3 stretches per day and build up
2. **Be Consistent**: Daily stretching, even just one stretch, is better than sporadic long sessions
3. **Trust the Algorithm**: The app learns your patterns and will suggest stretches you need most
4. **Use the Skip Button**: It's okay to skip stretches that don't feel right in the moment
5. **Check Your Progress**: Regular review of your statistics can be very motivating

## 🤝 Contributing

This is a personal project, but feel free to:
- Suggest new features
- Report bugs
- Share your customizations
- Contribute improvements

## 📝 License

This project is for personal use. Feel free to modify and adapt it to your needs!

---

**Happy Stretching! 🌟**

Remember: Consistency beats perfection. Every stretch counts toward a healthier, more flexible you!