# setup_credentials.R
# Run this ONCE to create credentials.sqlite from roster.R
# Rerun whenever roster changes (replaces the DB entirely)
# Run from the project root: Rscript setup_credentials.R

library(shinymanager)
source("roster.R")

if (file.exists("credentials.sqlite")) {
  file.remove("credentials.sqlite")
  message("Removed existing credentials.sqlite")
}

shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path      = "credentials.sqlite",
  passphrase       = "seagulls2026_db"
)

message("credentials.sqlite created with ", nrow(credentials), " users")
message("Players: jersey number is initial password (e.g. '12' for #12)")
message("Coaches: initial password is 'seagulls2026'")
message("All users can change their password via the login screen.")
