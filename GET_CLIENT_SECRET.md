# How to Download client_secret.json

Follow these exact steps to get your `client_secret.json` file:

## Step 1: Go to Google Cloud Console
1. Open your web browser
2. Go to: **https://console.cloud.google.com**
3. Sign in with your personal Google account

## Step 2: Create a Project (if you don't have one)
1. If you see "Select a project" at the top, click it
2. Click **"NEW PROJECT"**
3. Enter project name: **"Stretch Tracker"**
4. Click **"CREATE"**
5. Wait for the project to be created, then select it

## Step 3: Enable Required APIs
1. In the left menu, click **"APIs & Services"** → **"Library"**
2. Search for **"Google Drive API"** → Click on it → Click **"ENABLE"**
3. Search for **"Google Sheets API"** → Click on it → Click **"ENABLE"**

## Step 4: Configure OAuth Consent Screen
1. In the left menu, click **"APIs & Services"** → **"OAuth consent screen"**
2. Choose **"External"** → Click **"CREATE"**
3. Fill in the required fields:
   - **App name**: Stretch Tracker
   - **User support email**: Your email address
   - **Developer contact information**: Your email address
4. Click **"SAVE AND CONTINUE"**
5. Click **"SAVE AND CONTINUE"** on the Scopes page (leave empty)
6. Click **"SAVE AND CONTINUE"** on the Test users page (leave empty)
7. Click **"BACK TO DASHBOARD"**

## Step 5: Create OAuth 2.0 Credentials
1. In the left menu, click **"APIs & Services"** → **"Credentials"**
2. Click **"+ CREATE CREDENTIALS"** → **"OAuth 2.0 Client ID"**
3. Choose **"Web application"**
4. Enter name: **"Stretch Tracker Web Client"**
5. Under **"Authorized redirect URIs"**, click **"+ ADD URI"** and add:
   - `http://localhost:3838`
   - `https://stephnie.shinyapps.io/stretchies/`
6. Click **"CREATE"**

## Step 6: Download the JSON File
After you click "CREATE" in step 5, you'll see a popup window that says "OAuth client created". In this popup:

1. **Look for a download button** - it might say "DOWNLOAD JSON" or have a download icon (⬇️)
2. **Click the download button** - this will download a file to your Downloads folder
3. **The downloaded file will have a long name** like `client_secret_123456789-abcdefg.apps.googleusercontent.com.json`
4. **Find this file in your Downloads folder**
5. **Rename it to exactly: `client_secret.json`** (remove all the extra numbers and letters)
6. **Move this file to your app directory** (the same folder where `app.R` is located)

**Alternative if you don't see the download button:**
- Go back to **APIs & Services** → **Credentials**
- Find your "Stretch Tracker Web Client" in the list
- Click the download icon (⬇️) next to it
- This will download the JSON file

## Verify the File
Your `client_secret.json` file should:
- Be in the same folder as `app.R`
- Be named exactly `client_secret.json` (not `client_secret (1).json` or similar)
- Contain JSON data that looks like this:
```json
{
  "web": {
    "client_id": "your-client-id.googleusercontent.com",
    "project_id": "your-project-id",
    "auth_uri": "https://accounts.google.com/o/oauth2/auth",
    ...
  }
}
```

## Next Steps
Once you have `client_secret.json` in your app folder:
1. Run: `source("deploy_to_shinyapps.R")`
2. The deployment will now include Google Sheets integration!

## Troubleshooting
- **File not found**: Make sure the file is named exactly `client_secret.json`
- **Wrong location**: The file must be in the same folder as `app.R`
- **Permission errors**: Make sure you completed the OAuth consent screen setup