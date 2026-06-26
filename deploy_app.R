# deploy_app.R — publish the dashboard to shinyapps.io
#
# ONE-TIME SETUP (do this first, in the RStudio Console):
#   1. Create a free account at https://www.shinyapps.io
#   2. Dashboard -> Account -> Tokens -> Show, copy the line it gives you, e.g.
#        rsconnect::setAccountInfo(name="youracct", token="XXXX", secret="YYYY")
#      and run it once. (Stores your credentials locally.)
#
# THEN deploy (every time you want to push changes / new games live):
#   Rscript deploy_app.R
#
# Re-run this after new games land so the bundled fallback data stays fresh.

library(rsconnect)

# Files the live app needs. We DON'T deploy the whole folder — only these, so
# .git, RStudio cruft, setup scripts, and dev images stay out of the bundle.
app_files <- c(
  # App code
  "ui.R", "server.R", "global.R", "roster.R", "sync_drive.R",
  # Static assets (CSS, logo, player photos)
  "www",
  # Logins (shinymanager) — required for the password screen
  "credentials.sqlite",
  # Google Drive sync: folder id + cached auth token, so production pulls
  # live from Drive (and the coach "Sync Data" button works on the server).
  ".Renviron", ".secrets",
  # Raw game CSVs + combined file: instant data on boot; Drive re-syncs on top.
  "data", "all_fall_25.csv"
)

missing <- app_files[!file.exists(app_files)]
if (length(missing))
  stop("Missing required files for deploy: ", paste(missing, collapse = ", "))

rsconnect::deployApp(
  appDir      = ".",
  appName     = "sfs_dashboard",
  appTitle    = "SF Seagulls Dashboard",
  appFiles    = app_files,
  forceUpdate = TRUE,
  launch.browser = FALSE
)
