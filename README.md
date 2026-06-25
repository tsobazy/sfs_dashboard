# SF Seagulls Baseball Dashboard

Analytics dashboard for the SF Seagulls baseball team. Built with R Shiny.

## Setup

### 1. Install R packages

Open RStudio and run:

```r
install.packages(c(
  "shiny", "bslib", "shinyWidgets", "shinymanager",
  "plotly", "DT", "tidyverse", "lubridate", "scales"
))
```

### 2. Clone and open the project

```bash
git clone https://github.com/tsobazy/sfs_dashboard.git
```

Open `sfs_dashboard.Rproj` in RStudio.

### 3. Create your login credentials

Run this once from the RStudio console:

```r
source("setup_credentials.R")
```

This creates `credentials.sqlite` with the default logins. It's not included in the repo for security.

### 4. Run the app

Click **Run App** in RStudio, or run from the console:

```r
shiny::runApp()
```

---

## Adding game data

Real game CSVs go in `data/game_csvs/`. Drop Trackman export files in there and restart the app — they load automatically. They're also pulled automatically from the shared Google Drive folder on startup (see Drive sync).

Real combined data is written to `all_fall_25.csv`, which is **git-ignored** so real player data stays off the repo. Fresh clones fall back to the committed `sample_data.csv` so the app still runs out of the box.

---

## Files

| File | What it is |
|------|-----------|
| `global.R` | Data loading, helper functions, UI layout |
| `server.R` | All app logic and chart rendering |
| `ui.R` | App shell and login page |
| `roster.R` | Player roster and position info |
| `all_fall_25.csv` | Sample data for testing |
| `data/game_csvs/` | Real season CSVs go here |
| `setup_credentials.R` | Run once to create logins |
| `www/` | Logo, player photos, and CSS |
