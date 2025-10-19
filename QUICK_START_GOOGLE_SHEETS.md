# Quick Start: Google Sheets Integration for Personal Use

This is a simplified guide for connecting your personal Google account to your Stretch Tracker app.

## What You'll Get
- Your stretch data backed up to Google Sheets
- Access your data from anywhere
- Ability to manually edit data in Google Sheets
- Sync between multiple devices/deployments

## Step-by-Step Setup (15 minutes)

### 1. Create a Free Google Cloud Project
- Go to [console.cloud.google.com](https://console.cloud.google.com)
- Sign in with your personal Google account
- Click **"Create Project"**
- Name it something like "Stretch Tracker"
- Click **"Create"**

### 2. Enable APIs (2 clicks)
- In your new project, go to **APIs & Services** → **Library**
- Search for **"Google Drive API"** → Click **"Enable"**
- Search for **"Google Sheets API"** → Click **"Enable"**

### 3. Set Up OAuth Consent Screen
- Go to **APIs & Services** → **OAuth consent screen**
- Choose **"External"** (this is normal for personal use)
- Fill in:
  - **App name**: "Stretch Tracker"
  - **User support email**: Your email
  - **Developer contact**: Your email
- Click **"Save and Continue"** through the remaining steps

### 4. Create Credentials
- Go to **APIs & Services** → **Credentials**
- Click **"Create Credentials"** → **"OAuth 2.0 Client ID"**
- Choose **"Web application"**
- Add these **Authorized redirect URIs**:
  - `http://localhost:3838`
  - `https://YOUR-USERNAME.shinyapps.io/stretchies/` (replace YOUR-USERNAME)
- Click **"Create"**
- **Download the JSON file** and save it as `client_secret.json` in your app folder

### 5. Test Locally (Optional)
```r
# Install new packages
source("install_packages.R")

# Run the app
shiny::runApp()
```
- Go to Settings tab → try "Upload to Google Sheets"
- You'll be asked to sign in to Google

### 6. Deploy to shinyapps.io
```r
# Easy deployment
source("deploy_to_shinyapps.R")
```

### 7. First-Time Setup on shinyapps.io
- Visit your deployed app
- Go to Settings tab
- Click "Upload to Google Sheets"
- Sign in with Google when prompted
- Grant permissions

## That's It! 🎉

Your app will now:
- ✅ Sync data to Google Sheets automatically
- ✅ Create a spreadsheet called "Stretch_Tracker_Data"
- ✅ Allow you to view/edit data in Google Sheets
- ✅ Keep your data backed up in the cloud

## Troubleshooting

**"Access blocked" error?**
- Make sure you added your email as a test user in OAuth consent screen

**Can't find the JSON file?**
- It downloads to your Downloads folder
- Rename it to `client_secret.json`
- Move it to your app folder (same folder as `app.R`)

**App won't authenticate?**
- Check that your redirect URI matches your actual shinyapps.io URL
- Make sure both APIs are enabled

## Security Note
- Never share your `client_secret.json` file
- Don't commit it to GitHub or other public repositories
- This setup is perfect for personal use

## Need Help?
Check the detailed guide in `GOOGLE_SHEETS_SETUP.md` or the troubleshooting section there.