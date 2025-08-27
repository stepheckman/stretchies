# Install Required Packages for Stretch Tracker App
# Run this script once before running the app

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# List of required packages
required_packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "plotly",
  "shinyWidgets",
  "shinycssloaders",
  "dplyr",
  "ggplot2",
  "lubridate",
  "googledrive" # Added for Google Drive integration
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing package:", pkg, "\n")
      tryCatch({
        install.packages(pkg, dependencies = TRUE, quiet = FALSE)
        
        # Try to load the package after installation
        if (require(pkg, character.only = TRUE, quietly = TRUE)) {
          cat("Successfully installed and loaded:", pkg, "\n")
        } else {
          cat("ERROR: Failed to load package after installation:", pkg, "\n")
        }
      }, error = function(e) {
        cat("ERROR: Failed to install package:", pkg, "\n")
        cat("Error message:", e$message, "\n")
      })
    } else {
      cat("Package already installed:", pkg, "\n")
    }
  }
}

# Install missing packages
cat("=== Installing Required Packages for Stretch Tracker ===\n")
cat("Setting CRAN mirror to: https://cran.rstudio.com/\n")
install_if_missing(required_packages)

cat("\n=== Installation Complete! ===\n")
cat("You can now run the app with: shiny::runApp()\n")
cat("Or open app.R in RStudio and click 'Run App'\n")