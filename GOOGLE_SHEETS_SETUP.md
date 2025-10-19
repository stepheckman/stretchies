# Google Sheets Integration Setup Guide

This guide will walk you through setting up Google Sheets integration for your Stretch Tracker app on shinyapps.io.

## Prerequisites

- A personal Google account (the same one you use for Google Sheets)
- Your Stretch Tracker app ready for deployment

**Note**: You'll need to create a free Google Cloud Project, but this is just for API access - it's completely free for personal use and doesn't require a business account.

## Step 1: Create a Free Google Cloud Project

1. Go to [Google Cloud Console](https://console.cloud.google.com/)
2. Sign in with your personal Google account
3. If this is your first time:
   - Click **"Create Project"**
   - Enter a project name like "My Stretch Tracker"
   - Click **"Create"**
4. If you already have projects, click the project dropdown and create a new one

## Step 2: Enable APIs in Google Cloud Console

1. Make sure your new project is selected (check the project name at the top)
2. Navigate to **APIs & Services** → **Library**
3. Search for and enable these APIs:
   - **Google Drive API** - Click "Enable"
   - **Google Sheets API** - Click "Enable"

## Step 3: Configure OAuth Consent Screen

1. In Google Cloud Console, go to **APIs & Services** → **OAuth consent screen**
2. Choose **External** user type (unless you have a Google Workspace account)
3. Fill in the required information:
   - **App name**: "Stretch Tracker" (or your preferred name)
   - **User support email**: Your email address
   - **Developer contact information**: Your email address
4. Add scopes (optional for testing, but recommended):
   - `https://www.googleapis.com/auth/spreadsheets`
   - `https://www.googleapis.com/auth/drive.file`
5. Add test users:
   - Add your own email address to the test users list
   - This allows you to use the app while it's in testing mode

## Step 4: Create OAuth 2.0 Credentials

1. Go to **APIs & Services** → **Credentials**
2. Click **Create Credentials** → **OAuth 2.0 Client ID**
3. Choose **Web application**
4. Configure the client:
   - **Name**: "Stretch Tracker Web Client"
   - **Authorized JavaScript origins**: 
     - `http://localhost:3838` (for local testing)
     - `https://your-username.shinyapps.io` (replace with your actual shinyapps.io domain)
   - **Authorized redirect URIs**:
     - `http://localhost:3838`
     - `https://your-username.shinyapps.io/stretchies/` (replace with your actual app URL)

5. Click **Create**
6. **Download the JSON file** and save it as `client_secret.json` in your app directory

## Step 5: Test Locally (Optional but Recommended)

Before deploying to shinyapps.io, test the integration locally:

1. Place the `client_secret.json` file in your app directory
2. Install the new packages:
   ```r
   source("install_packages.R")
   ```
3. Run the app locally:
   ```r
   shiny::runApp()
   ```
4. Go to the Settings tab and test the Google Sheets sync functionality
5. You'll be prompted to authenticate with Google in your browser

## Step 6: Prepare for shinyapps.io Deployment

For shinyapps.io, we need to handle authentication differently since it's a server environment.

### Option A: Service Account (Recommended for Production)

1. In Google Cloud Console, go to **IAM & Admin** → **Service Accounts**
2. Create a new service account:
   - **Name**: "stretch-tracker-service"
   - **Description**: "Service account for Stretch Tracker app"
3. Grant the service account these roles:
   - **Editor** (or more restrictive custom role)
4. Create a key for the service account:
   - Click on the service account
   - Go to **Keys** tab
   - Click **Add Key** → **Create new key**
   - Choose **JSON** format
   - Download the key file

### Option B: User OAuth (Simpler for Personal Use)

For personal use, you can use the OAuth flow, but you'll need to authenticate interactively the first time the app runs on shinyapps.io.

## Step 7: Deploy to shinyapps.io

1. **Install rsconnect** if you haven't already:
   ```r
   install.packages("rsconnect")
   ```

2. **Configure your shinyapps.io account**:
   ```r
   library(rsconnect)
   rsconnect::setAccountInfo(name='your-account-name',
                            token='your-token',
                            secret='your-secret')
   ```

3. **Deploy the app**:
   ```r
   rsconnect::deployApp(appName = "stretchies")
   ```

## Step 8: First-Time Authentication on shinyapps.io

After deployment:

1. Visit your app URL: `https://your-username.shinyapps.io/stretchies/`
2. Go to the **Settings** tab
3. Click **"Upload to Google Sheets"**
4. You'll be redirected to Google for authentication
5. Grant the necessary permissions
6. You'll be redirected back to your app

## Step 9: Using Google Sheets Integration

Once authenticated, you can:

- **Upload to Google Sheets**: Sync your local data to Google Sheets
- **Download from Google Sheets**: Pull data from Google Sheets to your app
- **View your data**: The app will provide a link to your Google Sheets

## Troubleshooting

### Common Issues:

1. **"Access blocked" error**: 
   - Make sure your app is added to test users in OAuth consent screen
   - Verify the redirect URIs are correctly configured

2. **"Invalid client" error**:
   - Check that the `client_secret.json` file is correctly formatted
   - Verify the client ID matches your Google Cloud project

3. **Permission denied**:
   - Ensure the Google Drive and Sheets APIs are enabled
   - Check that the OAuth scopes include spreadsheets and drive access

4. **App won't start on shinyapps.io**:
   - Check the logs in your shinyapps.io dashboard
   - Ensure all required packages are installed
   - Verify file paths are correct

### Getting Help:

- Check the shinyapps.io logs for detailed error messages
- Verify your Google Cloud Console settings
- Test authentication locally first

## Security Notes

- Never commit `client_secret.json` to version control
- Consider using environment variables for sensitive data
- Regularly review and rotate your credentials
- Use service accounts for production deployments

## Data Structure in Google Sheets

The app will create a spreadsheet called "Stretch_Tracker_Data" with these sheets:
- **stretches**: Your stretch database
- **daily_stats**: Daily completion statistics  
- **stretch_history**: Complete history of stretch actions
- **user_preferences**: App settings and preferences

You can view and manually edit this data in Google Sheets if needed.