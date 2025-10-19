# Deploy Stretch Tracker to shinyapps.io
# Run this script to deploy your app to shinyapps.io

library(rsconnect)

# Check if rsconnect is configured
if (length(rsconnect::accounts()) == 0) {
  cat("❌ No shinyapps.io account configured.\n")
  cat("Please run the following command with your account details:\n")
  cat("rsconnect::setAccountInfo(name='your-account-name', token='your-token', secret='your-secret')\n")
  cat("\nYou can find these details at: https://www.shinyapps.io/admin/#/tokens\n")
  stop("Account configuration required")
}

# Display current account
current_account <- rsconnect::accounts()[1, ]
cat("📋 Deploying with account:", current_account$name, "\n")

# Check for required files
required_files <- c("app.R", "helpers.R", "data_setup.R", "google_sheets_helpers.R", "install_packages.R")
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  cat("❌ Missing required files:", paste(missing_files, collapse = ", "), "\n")
  stop("Please ensure all required files are present")
}

# Check for Google Sheets authentication files
has_service_account <- file.exists("service-account.json")
has_client_secret <- file.exists("client_secret.json")

if (!has_service_account && !has_client_secret) {
  cat("⚠️  Warning: No Google Sheets authentication files found.\n")
  cat("   - service-account.json (recommended for shinyapps.io)\n")
  cat("   - client_secret.json (for local development)\n")
  cat("   See SHINYAPPS_GOOGLE_SETUP.md for service account setup.\n")
  
  response <- readline("Continue deployment without Google Sheets? (y/n): ")
  if (tolower(response) != "y") {
    stop("Deployment cancelled")
  }
} else if (has_service_account) {
  cat("✅ Found service-account.json - Google Sheets will work on shinyapps.io\n")
} else if (has_client_secret) {
  cat("⚠️  Found client_secret.json but no service-account.json\n")
  cat("   OAuth may not work reliably on shinyapps.io\n")
  cat("   Consider creating a service account (see SHINYAPPS_GOOGLE_SETUP.md)\n")
  
  response <- readline("Continue with OAuth authentication? (y/n): ")
  if (tolower(response) != "y") {
    stop("Deployment cancelled")
  }
}

# Install packages locally first (recommended)
cat("📦 Installing packages locally...\n")
source("install_packages.R")

# Prepare deployment
cat("🚀 Preparing deployment...\n")

# Files to include in deployment
app_files <- c(
  "app.R",
  "helpers.R", 
  "data_setup.R",
  "google_sheets_helpers.R",
  "install_packages.R",
  "list.csv"
)

# Add authentication files if they exist
if (file.exists("service-account.json")) {
  app_files <- c(app_files, "service-account.json")
  cat("✅ Including service-account.json for Google Sheets integration\n")
}

if (file.exists("client_secret.json")) {
  app_files <- c(app_files, "client_secret.json")
  cat("✅ Including client_secret.json for Google Sheets integration\n")
}

# Add data directory if it exists
if (dir.exists("data")) {
  data_files <- list.files("data", full.names = TRUE)
  app_files <- c(app_files, data_files)
  cat("✅ Including", length(data_files), "data files\n")
}

# Deploy the app
cat("🚀 Deploying to shinyapps.io...\n")

tryCatch({
  rsconnect::deployApp(
    appName = "stretchies",
    appFiles = app_files,
    forceUpdate = TRUE,
    launch.browser = TRUE
  )
  
  cat("✅ Deployment successful!\n")
  cat("🌐 Your app should open in your browser shortly.\n")
  cat("📊 If Google Sheets integration is enabled, go to Settings tab to sync your data.\n")
  
}, error = function(e) {
  cat("❌ Deployment failed:", e$message, "\n")
  cat("\n🔧 Troubleshooting tips:\n")
  cat("1. Check your internet connection\n")
  cat("2. Verify your shinyapps.io account is active\n")
  cat("3. Ensure all required packages are available on CRAN\n")
  cat("4. Check the shinyapps.io dashboard for detailed logs\n")
})