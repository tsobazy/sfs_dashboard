# setup_drive_auth.R
# Run ONCE before first use: Rscript setup_drive_auth.R
# Opens a browser window for Google OAuth consent.
# Caches the token in .secrets/ so the app can authenticate silently.

library(googledrive)

dir.create(".secrets", showWarnings = FALSE)
options(gargle_oauth_cache = ".secrets")

# Clear any previously cached token so Google re-shows the consent screen.
# (A prior grant may have had insufficient scopes -> 403 on download.)
unlink(list.files(".secrets", full.names = TRUE))

# This opens a browser — follow the prompts to authorize.
# Request full read access to Drive files so the app can download CSVs.
# IMPORTANT: on the consent screen, leave every permission CHECKED.
drive_auth(
  cache  = ".secrets",
  scopes = "https://www.googleapis.com/auth/drive.readonly"
)

me <- drive_user()
cat("\nAuthenticated as:", me$emailAddress, "\n")
cat("Token cached in .secrets/ — you only need to run this once.\n\n")
cat("Next: set SEAGULLS_DRIVE_FOLDER_ID in .Renviron\n")
cat("  The folder ID is the last part of your Drive folder URL:\n")
cat("  https://drive.google.com/drive/folders/<FOLDER_ID_HERE>\n\n")
cat("Then restart the app and click 'Sync Data from Drive' in the coach sidebar.\n")
