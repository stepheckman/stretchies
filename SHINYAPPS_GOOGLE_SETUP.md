# Google Sheets Setup for shinyapps.io (Server Authentication)

The OAuth method doesn't work well on shinyapps.io. We need to use **Service Account** authentication instead.

## Step 1: Create a Service Account

1. Go to [Google Cloud Console](https://console.cloud.google.com)
2. Select your "Stretch Tracker" project
3. Go to **IAM & Admin** → **Service Accounts**
4. Click **"+ CREATE SERVICE ACCOUNT"**
5. Fill in:
   - **Service account name**: `stretch-tracker-service`
   - **Description**: `Service account for Stretch Tracker app`
6. Click **"CREATE AND CONTINUE"**
7. For roles, add:
   - **Editor** (or you can be more specific with just Drive/Sheets permissions)
8. Click **"CONTINUE"** then **"DONE"**

## Step 2: Create and Download Service Account Key

1. Click on your new service account (`stretch-tracker-service`)
2. Go to the **"Keys"** tab
3. Click **"ADD KEY"** → **"Create new key"**
4. Choose **"JSON"** format
5. Click **"CREATE"**
6. The key file will download automatically
7. **Rename this file to `service-account.json`**
8. **Move it to your app directory** (same folder as `app.R`)

## Step 3: Share Your Google Sheets with the Service Account

1. Open the downloaded `service-account.json` file
2. Find the `"client_email"` field - it looks like: `stretch-tracker-service@your-project.iam.gserviceaccount.com`
3. **Copy this email address**
4. When your app creates Google Sheets, you'll need to share them with this email address

## Step 4: Update Your App

I'll create an updated version that uses service account authentication for shinyapps.io.

## Step 5: Deploy

Once you have `service-account.json` in your app folder:
```r
source("deploy_to_shinyapps.R")
```

## Why Service Accounts Work Better

- ✅ No interactive authentication required
- ✅ Works reliably on server environments
- ✅ More secure for production apps
- ✅ No user intervention needed after deployment

## Security Note

- Keep your `service-account.json` file secure
- Don't commit it to version control
- The service account only has access to sheets you explicitly share with it