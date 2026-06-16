# SF Seagulls Dashboard Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a role-based R Shiny dashboard for the SF Seagulls with a full coach view (14 charts) and a personal process-stats player view, protected by `shinymanager` login.

**Architecture:** `global.R` (already complete) loads data and defines metric functions. `roster.R` (new) holds credentials and position lookups. `ui.R` wraps everything in `secure_app()` and renders a `uiOutput("main_ui")` that the server swaps between coach and player layouts. `server.R` handles auth reactives, the master `fdata()` filter, all 14 coach charts, the player game browser, and player scorecards.

**Tech Stack:** R, Shiny, bslib, shinymanager, shinyWidgets, tidyverse, plotly, DT, lubridate, scales, testthat

## Global Constraints

- White background everywhere: `bg="white"` in bslib theme, `plotly_white()` on every plotly chart, `theme_seagulls()` on every ggplot
- `PITCH_COLORS` from `global.R` must be used for all pitch-type coloring
- Strike zone constants: `SZ_LEFT=-0.83`, `SZ_RIGHT=0.83`, `SZ_BOT=1.50`, `SZ_TOP=3.50`
- All `renderPlotly` / `renderDataTable` calls must begin with `req(nrow(fdata()) > 0)`
- Player view: never expose outcome stats (AVG, OBP, SLG, K%, BB%) to players
- Player view: never expose other players' data — `player_fdata()` filters by logged-in player only
- Coach view: unchanged from original spec except Position column in leaderboards and header/logout
- `all_fall_25.csv` must be in project root at runtime (already loaded in `global.R`)
- Default player password = jersey number as string; default coach password = `"seagulls2026"`
- Score tiles use teal `#2A9D8F` for strong, amber `#F4A261` for areas to watch — no red in player view

---

## File Map

| File | Action | Responsibility |
|---|---|---|
| `global.R` | Already complete | Packages, data, constants, metric functions, theme |
| `roster.R` | **Create** | Credentials data frame + position lookup table |
| `tests/testthat.R` | **Create** | testthat runner |
| `tests/testthat/test-metrics.R` | **Create** | Unit tests for all metric functions |
| `ui.R` | **Create** | `secure_app` wrapper, coach sidebar, tab layout, player layout functions |
| `server.R` | **Create** | Auth reactives, fdata, all chart renders, player view renders |

---

## Task 1: Install shinymanager + create roster.R + test suite

**Files:**
- Create: `roster.R`
- Create: `tests/testthat.R`
- Create: `tests/testthat/test-metrics.R`

**Interfaces:**
- Produces: `credentials` data frame (consumed by Task 3's `check_credentials()`)
- Produces: `roster_positions` data frame (consumed by Task 4 and Task 6 leaderboards)

- [ ] **Step 1: Install shinymanager**

In an R console inside the project:
```r
install.packages("shinymanager")
install.packages("testthat")
```

- [ ] **Step 2: Create `roster.R`**

```r
# roster.R — credentials and position lookup for SF Seagulls 2026

credentials <- data.frame(
  user = c(
    # Coaches
    "coach_cascone", "coach_dumlao", "coach_ferreira", "coach_medina",
    "coach_aranibar", "coach_ballelos", "coach_caviglia", "coach_frediani",
    "coach_rodarte",
    # Players
    "bryce_brooks", "sebastian_ultreras", "declan_mendel", "emilio_feliciano",
    "davis_germann", "theodore_tsouras", "benjamin_joost", "finn_whalen",
    "matthew_potter", "louden_hilliard", "caid_heflin", "jacob_gilbreath",
    "joseph_steidel", "blake_cowans", "jake_brewer", "ethan_lopez",
    "caleb_garrison", "marcus_graham", "connor_wood", "taylor_easthope",
    "derek_waldvogel", "tanner_wall", "armando_hurtado", "brandon_swanson",
    "christian_lamothe", "jb_ferreira", "kai_hanasaki", "branson_derrington",
    "camren_boyd", "luka_shah", "alan_ramirez"
  ),
  password = c(
    # Coaches (all default to "seagulls2026")
    rep("seagulls2026", 9),
    # Players (default = jersey number as string)
    "1", "3", "6", "7", "9", "12", "13", "14", "16", "17", "20", "22",
    "25", "26", "29", "30", "31", "32", "34", "35", "36", "37", "38",
    "39", "40", "43", "50", "53", "54", "55", "56"
  ),
  role = c(
    rep("coach", 9),
    rep("player", 31)
  ),
  display_name = c(
    "Cage Cascone", "Dominic Dumlao", "Bill Ferreira", "Andres Medina",
    "Jorge Aranibar", "Eric Ballelos", "Marc Caviglia", "Danielle Frediani",
    "Ashley Rodarte",
    "Bryce Brooks", "Sebastian Ultreras", "Declan Mendel", "Emilio Feliciano",
    "Davis Germann", "Theodore Tsouras", "Benjamin Joost", "Finn Whalen",
    "Matthew Potter", "Louden Hilliard", "Caid Heflin", "Jacob Gilbreath",
    "Joseph Steidel", "Blake Cowans", "Jake Brewer", "Ethan Lopez",
    "Caleb Garrison", "Marcus Graham", "Connor Wood", "Taylor Easthope",
    "Derek Waldvogel", "Tanner Wall", "Armando Hurtado", "Brandon Swanson",
    "Christian LaMothe", "JB Ferreira", "Kai Hanasaki", "Branson Derrington",
    "Camren Boyd", "Luka Shah", "Alan Ramirez"
  ),
  player_name = c(
    rep(NA_character_, 9),
    "Bryce Brooks", "Sebastian Ultreras", "Declan Mendel", "Emilio Feliciano",
    "Davis Germann", "Theodore Tsouras", "Benjamin Joost", "Finn Whalen",
    "Matthew Potter", "Louden Hilliard", "Caid Heflin", "Jacob Gilbreath",
    "Joseph Steidel", "Blake Cowans", "Jake Brewer", "Ethan Lopez",
    "Caleb Garrison", "Marcus Graham", "Connor Wood", "Taylor Easthope",
    "Derek Waldvogel", "Tanner Wall", "Armando Hurtado", "Brandon Swanson",
    "Christian LaMothe", "JB Ferreira", "Kai Hanasaki", "Branson Derrington",
    "Camren Boyd", "Luka Shah", "Alan Ramirez"
  ),
  player_type = c(
    rep(NA_character_, 9),
    "hitter", "hitter", "pitcher", "hitter", "hitter",
    "pitcher", "pitcher", "pitcher", "pitcher", "pitcher",
    "hitter", "hitter", "pitcher", "hitter", "hitter",
    "hitter", "pitcher", "hitter", "pitcher", "pitcher",
    "hitter", "hitter", "hitter", "hitter", "hitter",
    "pitcher", "pitcher", "pitcher", "pitcher", "pitcher",
    "hitter"
  ),
  jersey = c(
    rep(NA_integer_, 9),
    1L, 3L, 6L, 7L, 9L, 12L, 13L, 14L, 16L, 17L, 20L, 22L,
    25L, 26L, 29L, 30L, 31L, 32L, 34L, 35L, 36L, 37L, 38L,
    39L, 40L, 43L, 50L, 53L, 54L, 55L, 56L
  ),
  stringsAsFactors = FALSE
)

roster_positions <- data.frame(
  player_name = c(
    "Bryce Brooks", "Sebastian Ultreras", "Declan Mendel", "Emilio Feliciano",
    "Davis Germann", "Theodore Tsouras", "Benjamin Joost", "Finn Whalen",
    "Matthew Potter", "Louden Hilliard", "Caid Heflin", "Jacob Gilbreath",
    "Joseph Steidel", "Blake Cowans", "Jake Brewer", "Ethan Lopez",
    "Caleb Garrison", "Marcus Graham", "Connor Wood", "Taylor Easthope",
    "Derek Waldvogel", "Tanner Wall", "Armando Hurtado", "Brandon Swanson",
    "Christian LaMothe", "JB Ferreira", "Kai Hanasaki", "Branson Derrington",
    "Camren Boyd", "Luka Shah", "Alan Ramirez"
  ),
  position = c(
    "INF", "INF", "RHP", "INF", "OF",
    "LHP", "RHP", "RHP", "RHP", "RHP",
    "OF", "OF", "LHP", "C", "INF",
    "C", "RHP", "C/OF", "RHP", "LHP",
    "1B/OF", "INF/OF", "C/OF", "INF", "INF",
    "RHP", "RHP", "RHP", "LHP", "RHP",
    "INF"
  ),
  jersey = c(
    1L, 3L, 6L, 7L, 9L, 12L, 13L, 14L, 16L, 17L, 20L, 22L,
    25L, 26L, 29L, 30L, 31L, 32L, 34L, 35L, 36L, 37L, 38L,
    39L, 40L, 43L, 50L, 53L, 54L, 55L, 56L
  ),
  stringsAsFactors = FALSE
)
```

- [ ] **Step 3: Create `tests/testthat.R`**

```r
library(testthat)
source("../global.R")
test_check("sfs_dashboard")
```

- [ ] **Step 4: Create `tests/testthat/test-metrics.R`**

```r
library(testthat)

# Source global to get metric functions and constants
source(file.path(dirname(getwd()), "global.R"), local = TRUE)

test_that("strike_pct counts strike outcomes correctly", {
  calls <- c("StrikeCalled", "BallCalled", "InPlay", "StrikeSwinging",
             "FoulBallNotFieldable", "BallCalled")
  expect_equal(strike_pct(calls), 4/6)
})

test_that("strike_pct returns 0 for all balls", {
  expect_equal(strike_pct(c("BallCalled", "BallCalled")), 0)
})

test_that("whiff_pct returns swinging-strike rate among swings", {
  calls <- c("StrikeSwinging", "InPlay", "FoulBallNotFieldable", "BallCalled")
  # swings = StrikeSwinging + InPlay + FoulBallNotFieldable = 3
  # whiffs = StrikeSwinging = 1
  expect_equal(whiff_pct(calls), 1/3)
})

test_that("whiff_pct returns NA when no swings", {
  expect_true(is.na(whiff_pct(c("BallCalled", "BallCalled"))))
})

test_that("csw_pct counts called + swinging strikes", {
  calls <- c("StrikeCalled", "StrikeSwinging", "BallCalled", "InPlay")
  expect_equal(csw_pct(calls), 2/4)
})

test_that("chase_pct returns swing rate on out-of-zone pitches", {
  side   <- c(1.5, 0.0, -1.5, 0.0)   # 1st and 3rd are OOZ
  height <- c(2.5, 2.5,  2.5, 0.5)   # 4th is OOZ (below SZ_BOT)
  calls  <- c("StrikeSwinging", "BallCalled", "BallCalled", "InPlay")
  # OOZ pitches: index 1 (side=1.5 > SZ_RIGHT=0.83) and index 4 (height=0.5 < SZ_BOT=1.50)
  # Swings on OOZ: index 1 (StrikeSwinging), index 4 (InPlay) = 2 swings / 2 OOZ pitches
  expect_equal(chase_pct(side, height, calls), 1.0)
})

test_that("chase_pct returns NA when no OOZ pitches", {
  side   <- c(0.0, 0.0)
  height <- c(2.5, 2.5)
  calls  <- c("StrikeSwinging", "BallCalled")
  expect_true(is.na(chase_pct(side, height, calls)))
})

test_that("hard_hit_pct calculates correctly", {
  ev <- c(97, 85, 100, 90, 95)
  # >= 95: indices 1, 3, 5 = 3 of 5
  expect_equal(hard_hit_pct(ev), 3/5)
})

test_that("hard_hit_pct ignores NAs", {
  ev <- c(97, NA, 85, 95)
  expect_equal(hard_hit_pct(ev), 2/3)
})

test_that("barrel_pct identifies barrels correctly", {
  ev <- c(100, 85, 99, 100)
  la <- c(28,  28,  10,  28)
  # Barrel: ev >= 98 AND la >= 26 AND la <= 30
  # Index 1: 100 >= 98, 28 in [26,30] → barrel
  # Index 3: 99 >= 98, 10 not in [26,30] → not barrel
  # Index 4: 100 >= 98, 28 in [26,30] → barrel
  expect_equal(barrel_pct(ev, la), 2/4)
})

test_that("gb_pct calculates ground ball rate among BIP", {
  ht <- c("GroundBall", "FlyBall", "LineDrive", "GroundBall", "Popup")
  expect_equal(gb_pct(ht), 2/5)
})

test_that("gb_pct returns NA when no BIP", {
  expect_true(is.na(gb_pct(c("Undefined", "Undefined"))))
})
```

- [ ] **Step 5: Run the tests**

In R console:
```r
setwd("/Users/tsobazy/sfs_dashboard")
testthat::test_dir("tests/testthat")
```

Expected: All 11 tests pass. If any fail, fix the test data (the functions in `global.R` are the source of truth).

- [ ] **Step 6: Commit**

```bash
git add roster.R tests/testthat.R tests/testthat/test-metrics.R
git commit -m "feat: add roster credentials, position lookup, and metric unit tests"
```

---

## Task 2: Create `ui.R`

**Files:**
- Create: `ui.R`

**Interfaces:**
- Consumes: `credentials` from `roster.R` (via `global.R` source)
- Produces: All `inputId` names used in Task 3's server reactives:
  - `"view_mode"`, `"player"`, `"dates"`, `"pitch_types"`, `"count"`, `"innings"`
  - `"prev_game"`, `"next_game"` (player view buttons)
  - `"logout"` (both views)
- Produces: All `outputId` names referenced in Tasks 3–10:
  - `"main_ui"` (role router)
  - Pitching: `"plot_zone"`, `"plot_arsenal"`, `"plot_velo_spin"`, `"table_pitchers"`, `"plot_movement"`, `"plot_release"`, `"plot_outcomes"`, `"plot_count_heatmap"`
  - Hitting: `"plot_spray"`, `"plot_ev_la"`, `"table_batters"`, `"plot_swing_zones"`, `"plot_hit_types"`, `"plot_pitch_vuln"`
  - Insight: `"insights"`
  - Player: `"player_ui"` (nested inside `"main_ui"`)

- [ ] **Step 1: Create `ui.R`**

```r
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinymanager)
library(plotly)
library(DT)

source("roster.R")

# ── Coach layout helpers ───────────────────────────────────────────────────────

coach_sidebar <- function() {
  div(
    style = "width:280px; min-width:280px; padding:16px; background:#f8f9fa;
             border-right:1px solid #e0e0e0; height:100vh; overflow-y:auto;",
    div(
      style = "margin-bottom:12px; padding-bottom:12px; border-bottom:1px solid #ddd;",
      tags$small(textOutput("coach_header", inline = TRUE), style = "color:#555;"),
      actionButton("logout", "Log Out", class = "btn-sm btn-outline-secondary mt-1")
    ),
    radioGroupButtons(
      "view_mode", label = "View",
      choices = c("Pitching", "Hitting"),
      selected = "Pitching", justified = TRUE, size = "sm"
    ),
    hr(),
    pickerInput(
      "player", "Player",
      choices = c("All Players"),
      options = list(`live-search` = TRUE)
    ),
    dateRangeInput("dates", "Date Range", start = NULL, end = NULL),
    pickerInput(
      "pitch_types", "Pitch Types",
      choices = NULL, multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    ),
    selectInput("count", "Count",
      choices = c("All","0-0","0-1","0-2","1-0","1-1","1-2",
                  "2-0","2-1","2-2","3-0","3-1","3-2")
    ),
    sliderInput("innings", "Innings", min = 1, max = 9,
                value = c(1, 9), step = 1),
    hr(),
    uiOutput("insights")
  )
}

coach_layout <- function() {
  div(
    style = "display:flex; height:100vh;",
    coach_sidebar(),
    div(
      style = "flex:1; overflow-y:auto; padding:20px;",
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Pitching",
          fluidRow(
            column(6, plotlyOutput("plot_zone",    height = "380px")),
            column(6, plotlyOutput("plot_arsenal", height = "380px"))
          ),
          fluidRow(
            column(12, plotlyOutput("plot_velo_spin", height = "340px"))
          ),
          fluidRow(
            column(12, DTOutput("table_pitchers"))
          ),
          fluidRow(
            column(6, plotlyOutput("plot_movement", height = "380px")),
            column(6, plotlyOutput("plot_release",  height = "380px"))
          ),
          fluidRow(
            column(6, plotlyOutput("plot_outcomes",      height = "360px")),
            column(6, plotlyOutput("plot_count_heatmap", height = "360px"))
          )
        ),
        tabPanel(
          "Hitting",
          fluidRow(
            column(6, plotlyOutput("plot_spray", height = "420px")),
            column(6, plotlyOutput("plot_ev_la", height = "420px"))
          ),
          fluidRow(
            column(12, DTOutput("table_batters"))
          ),
          fluidRow(
            column(4, plotlyOutput("plot_swing_zones", height = "360px")),
            column(4, plotlyOutput("plot_hit_types",   height = "360px")),
            column(4, plotlyOutput("plot_pitch_vuln",  height = "360px"))
          )
        )
      )
    )
  )
}

# ── Player layout (rendered server-side via uiOutput("player_ui")) ─────────────
# The actual player_ui is built in server.R using renderUI because it needs
# auth info (player name, jersey, position). This file only defines the shell.

player_shell <- function() {
  div(
    style = "max-width:900px; margin:0 auto; padding:16px;",
    uiOutput("player_ui")
  )
}

# ── App UI ─────────────────────────────────────────────────────────────────────

ui <- secure_app(
  fluidPage(
    theme = bs_theme(bg = "white", fg = "#0a1628", primary = "#0a1628",
                     version = 5),
    tags$head(tags$style(HTML("
      body { font-family: 'Helvetica Neue', Arial, sans-serif; }
      .value-tile {
        background: white; border: 1px solid #e0e0e0; border-radius: 8px;
        padding: 16px; text-align: center; margin-bottom: 12px;
      }
      .value-tile .tile-label { font-size: 12px; color: #666; margin-bottom: 4px; }
      .value-tile .tile-value { font-size: 28px; font-weight: bold; }
      .tile-teal  { color: #2A9D8F; }
      .tile-amber { color: #F4A261; }
      .tile-neutral { color: #0a1628; }
      .game-nav { display:flex; align-items:center; gap:12px; margin-bottom:16px; }
      .game-nav .game-label { font-size:15px; font-weight:600; color:#0a1628; }
    "))),
    uiOutput("main_ui")
  )
)
```

- [ ] **Step 2: Verify ui.R loads without error**

In R console:
```r
setwd("/Users/tsobazy/sfs_dashboard")
source("global.R")
source("roster.R")
source("ui.R")
# Should produce no errors. The `ui` object will exist.
cat("ui.R loaded OK\n")
```

Expected: No error messages, `ui` object exists.

- [ ] **Step 3: Commit**

```bash
git add ui.R
git commit -m "feat: add ui.R with coach layout, player shell, and shinymanager wrapper"
```

---

## Task 3: `server.R` — auth reactives, fdata, and view router

**Files:**
- Create: `server.R`

**Interfaces:**
- Consumes: `credentials` from `roster.R`, `data` from `global.R`
- Consumes: inputs `"view_mode"`, `"player"`, `"dates"`, `"pitch_types"`, `"count"`, `"innings"`
- Produces: `fdata()` reactive (consumed by Tasks 4–7)
- Produces: `player_fdata()` reactive (consumed by Tasks 9–10)
- Produces: `user_role()`, `user_player_name()`, `user_player_type()` reactives (consumed by Tasks 8–10)
- Produces: `output$main_ui`, `output$coach_header` (rendered in this task)

- [ ] **Step 1: Create `server.R` with auth + fdata + router**

```r
library(shiny)
library(shinymanager)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(scales)

source("roster.R")

server <- function(input, output, session) {

  # ── Auth ──────────────────────────────────────────────────────────────────
  result_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  user_role <- reactive({
    req(result_auth()$user_auth)
    result_auth()$role
  })

  user_player_name <- reactive({
    req(result_auth()$user_auth)
    result_auth()$player_name
  })

  user_player_type <- reactive({
    req(result_auth()$user_auth)
    result_auth()$player_type
  })

  user_display_name <- reactive({
    req(result_auth()$user_auth)
    result_auth()$display_name
  })

  # ── Coach header ──────────────────────────────────────────────────────────
  output$coach_header <- renderText({
    paste0("Logged in as: ", user_display_name(), " (Coach)")
  })

  observeEvent(input$logout, {
    session$reload()
  })

  # ── Main UI router ─────────────────────────────────────────────────────────
  output$main_ui <- renderUI({
    req(user_role())
    if (user_role() == "coach") {
      coach_layout()
    } else {
      player_shell()
    }
  })

  # ── Coach: dynamic filter initialisation ──────────────────────────────────
  observe({
    req(user_role() == "coach")
    req(input$view_mode)

    # Date range bounds from data
    updateDateRangeInput(session, "dates",
      start = min(data$Date, na.rm = TRUE),
      end   = max(data$Date, na.rm = TRUE)
    )

    # Pitch types
    pt <- sort(unique(data$TaggedPitchType[data$TaggedPitchType != "Undefined"]))
    updatePickerInput(session, "pitch_types",
      choices = pt, selected = pt
    )

    # Player list switches on view_mode
    players <- if (input$view_mode == "Pitching") {
      sort(unique(data$Pitcher))
    } else {
      sort(unique(data$Batter))
    }
    updatePickerInput(session, "player",
      choices = c("All Players", players),
      selected = "All Players"
    )
  })

  # ── Master filtered reactive (coach view) ─────────────────────────────────
  fdata <- reactive({
    req(user_role() == "coach")
    req(input$dates, input$pitch_types, input$count, input$innings)

    d <- data %>%
      filter(
        Date >= input$dates[1],
        Date <= input$dates[2],
        TaggedPitchType %in% input$pitch_types,
        TaggedPitchType != "Undefined",
        Inning >= input$innings[1],
        Inning <= input$innings[2]
      )

    if (input$count != "All") {
      parts <- strsplit(input$count, "-")[[1]]
      b <- as.integer(parts[1]); s <- as.integer(parts[2])
      d <- d %>% filter(Balls == b, Strikes == s)
    }

    if (!is.null(input$player) && input$player != "All Players") {
      if (input$view_mode == "Pitching") {
        d <- d %>% filter(Pitcher == input$player)
      } else {
        d <- d %>% filter(Batter == input$player)
      }
    }
    d
  })

  # ── Player filtered reactive ───────────────────────────────────────────────
  # Defined here; game index and selection live in Task 8.
  # This reactive is extended in Task 8 with selected_game_index.
  player_fdata_base <- reactive({
    req(user_role() == "player")
    req(user_player_name())
    ptype <- user_player_type()
    name  <- user_player_name()
    col   <- if (ptype == "pitcher") "Pitcher" else "Batter"
    data %>%
      filter(.data[[col]] == name, TaggedPitchType != "Undefined")
  })

}
```

- [ ] **Step 2: Verify the app starts**

```r
setwd("/Users/tsobazy/sfs_dashboard")
shiny::runApp(".", launch.browser = FALSE, port = 3838)
```

Expected: App starts, login screen appears at `http://localhost:3838`. Log in with `coach_cascone` / `seagulls2026`. Should see a blank coach layout (no charts yet — outputs are defined in later tasks). No R errors in the console.

Stop the app with Ctrl+C.

- [ ] **Step 3: Commit**

```bash
git add server.R
git commit -m "feat: add server.R with shinymanager auth, fdata reactive, and view router"
```

---

## Task 4: Coach pitching tab — Charts 1–4

**Files:**
- Modify: `server.R` (append chart renders inside `server` function)

**Interfaces:**
- Consumes: `fdata()` from Task 3, `PITCH_COLORS`, `theme_seagulls()`, `plotly_white()` from `global.R`
- Consumes: `roster_positions` from Task 1's `roster.R`
- Produces: `output$plot_zone`, `output$plot_arsenal`, `output$plot_velo_spin`, `output$table_pitchers`

- [ ] **Step 1: Append Chart 1 — Strike Zone Map to `server.R`**

Inside the `server <- function(...) {` block, after the `player_fdata_base` reactive, add:

```r
  # ── Chart 1: Strike Zone Map ───────────────────────────────────────────────
  output$plot_zone <- renderPlotly({
    req(nrow(fdata()) > 0)
    p <- ggplot(fdata(), aes(
        x = PlateLocSide, y = PlateLocHeight,
        color = TaggedPitchType,
        text = paste0("Pitcher: ", Pitcher,
                      "<br>Type: ", TaggedPitchType,
                      "<br>Speed: ", round(RelSpeed, 1), " mph",
                      "<br>Result: ", PitchCall)
      )) +
      geom_point(alpha = 0.55, size = 2) +
      annotate("rect",
        xmin = SZ_LEFT, xmax = SZ_RIGHT,
        ymin = SZ_BOT,  ymax = SZ_TOP,
        fill = NA, color = "black", linewidth = 0.8
      ) +
      scale_color_manual(values = PITCH_COLORS, drop = FALSE) +
      scale_x_continuous(limits = c(-2.5, 2.5)) +
      scale_y_continuous(limits = c(0, 5)) +
      labs(
        title    = "Pitch Location Map",
        subtitle = "Each dot is one pitch. The black box is the strike zone.",
        color    = NULL, x = "Horizontal (ft)", y = "Height (ft)"
      ) +
      theme_seagulls() + coord_fixed()
    plotly_white(ggplotly(p, tooltip = "text"))
  })
```

- [ ] **Step 2: Append Chart 2 — Pitch Arsenal Donut**

```r
  # ── Chart 2: Pitch Arsenal Donut ──────────────────────────────────────────
  output$plot_arsenal <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      group_by(TaggedPitchType) %>%
      summarise(
        n       = n(),
        avg_spd = round(mean(RelSpeed, na.rm = TRUE), 1),
        avg_spin = round(mean(SpinRate, na.rm = TRUE), 0),
        .groups = "drop"
      ) %>%
      mutate(pct = n / sum(n))

    plot_ly(d,
      labels = ~TaggedPitchType, values = ~n,
      type   = "pie", hole = 0.5,
      marker = list(colors = unname(PITCH_COLORS[d$TaggedPitchType])),
      text   = ~paste0(TaggedPitchType, "<br>", n, " pitches (",
                       scales::percent(pct, accuracy = 1), ")<br>",
                       avg_spd, " mph | ", avg_spin, " rpm"),
      hoverinfo = "text",
      textinfo  = "label+percent"
    ) %>%
      layout(
        title       = list(text = "Pitch Arsenal", font = list(color = "#0a1628")),
        showlegend  = FALSE,
        paper_bgcolor = "white", plot_bgcolor = "white",
        font = list(color = "#0a1628")
      )
  })
```

- [ ] **Step 3: Append Chart 3 — Velocity & Spin Subplot**

```r
  # ── Chart 3: Velocity & Spin by Pitch Type ────────────────────────────────
  output$plot_velo_spin <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      group_by(TaggedPitchType) %>%
      summarise(
        avg_spd  = mean(RelSpeed,  na.rm = TRUE),
        avg_spin = mean(SpinRate,  na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      arrange(avg_spd)

    cols <- PITCH_COLORS[d$TaggedPitchType]

    p_spd <- ggplot(d, aes(
        x = avg_spd, y = reorder(TaggedPitchType, avg_spd),
        fill = TaggedPitchType
      )) +
      geom_col() +
      geom_text(aes(label = round(avg_spd, 1)), hjust = -0.1, size = 3) +
      scale_fill_manual(values = PITCH_COLORS) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Avg Velocity (mph)", y = NULL, title = "Avg Velocity") +
      theme_seagulls() + theme(legend.position = "none")

    p_spin <- ggplot(d, aes(
        x = avg_spin, y = reorder(TaggedPitchType, avg_spd),
        fill = TaggedPitchType
      )) +
      geom_col() +
      geom_text(aes(label = round(avg_spin, 0)), hjust = -0.1, size = 3) +
      scale_fill_manual(values = PITCH_COLORS) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Avg Spin Rate (rpm)", y = NULL, title = "Avg Spin Rate") +
      theme_seagulls() + theme(legend.position = "none")

    subplot(
      plotly_white(ggplotly(p_spd,  tooltip = "none")),
      plotly_white(ggplotly(p_spin, tooltip = "none")),
      nrows = 1, shareY = TRUE, titleX = TRUE
    ) %>% layout(paper_bgcolor = "white", plot_bgcolor = "white",
                  font = list(color = "#0a1628"))
  })
```

- [ ] **Step 4: Append Chart 4 — Pitcher Leaderboard**

```r
  # ── Chart 4: Pitcher Leaderboard ──────────────────────────────────────────
  output$table_pitchers <- renderDT({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      group_by(Pitcher) %>%
      summarise(
        Pitches  = n(),
        `Strike%` = strike_pct(PitchCall),
        `Whiff%`  = whiff_pct(PitchCall),
        `CSW%`    = csw_pct(PitchCall),
        `Avg Velo` = mean(RelSpeed, na.rm = TRUE),
        BF = n_distinct(paste(Date, Inning, PAofInning)),
        K  = sum(KorBB == "Strikeout", na.rm = TRUE),
        BB = sum(KorBB == "Walk",      na.rm = TRUE),
        GB_pct = gb_pct(TaggedHitType),
        .groups = "drop"
      ) %>%
      mutate(
        `K%`  = K  / BF,
        `BB%` = BB / BF,
        `GB%` = GB_pct
      ) %>%
      left_join(roster_positions[, c("player_name", "position")],
                by = c("Pitcher" = "player_name")) %>%
      select(Pitcher, Position = position, Pitches,
             `Strike%`, `Whiff%`, `CSW%`, `Avg Velo`, `K%`, `BB%`, `GB%`)

    dt <- datatable(d,
      options = list(
        pageLength = 10,
        order      = list(list(4, "desc"))   # sort by Strike% col index (0-based: col 3 → 4 w/ position)
      ),
      rownames = FALSE
    ) %>%
      formatPercentage(c("Strike%","Whiff%","CSW%","K%","BB%","GB%"), digits = 1) %>%
      formatRound("Avg Velo", digits = 1) %>%
      formatStyle("Strike%",
        background = styleInterval(c(0.54, 0.65),
          c("#fde8e8", "white", "#e0f5f2"))
      ) %>%
      formatStyle("Whiff%",
        background = styleInterval(c(0.19, 0.30),
          c("#fde8e8", "white", "#e0f5f2"))
      )
    dt
  })
```

- [ ] **Step 5: Run the app and verify charts 1–4**

```r
shiny::runApp(".", launch.browser = TRUE)
```

Log in as `coach_cascone` / `seagulls2026`. Check Pitching tab:
- Chart 1: Colored dots appear with strike zone box
- Chart 2: Donut with pitch type breakdown
- Chart 3: Side-by-side horizontal bars
- Chart 4: Table with Position column, sortable, percentage formatting

Stop with Ctrl+C.

- [ ] **Step 6: Commit**

```bash
git add server.R
git commit -m "feat: add pitching charts 1–4 (zone map, arsenal, velo/spin, leaderboard)"
```

---

## Task 5: Coach pitching tab — Charts 5–8

**Files:**
- Modify: `server.R` (append inside `server` function)

**Interfaces:**
- Consumes: `fdata()`, `PITCH_COLORS`, `theme_seagulls()`, `plotly_white()`
- Produces: `output$plot_movement`, `output$plot_release`, `output$plot_outcomes`, `output$plot_count_heatmap`

- [ ] **Step 1: Append Chart 5 — Pitch Movement Scatter**

```r
  # ── Chart 5: Pitch Movement Scatter ───────────────────────────────────────
  output$plot_movement <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      group_by(TaggedPitchType) %>%
      summarise(
        HorzBreak       = mean(HorzBreak,        na.rm = TRUE),
        InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
        n               = n(),
        .groups = "drop"
      )

    p <- ggplot(d, aes(
        x = HorzBreak, y = InducedVertBreak,
        color = TaggedPitchType, size = n,
        label = TaggedPitchType
      )) +
      geom_hline(yintercept = 0, color = "#cccccc") +
      geom_vline(xintercept = 0, color = "#cccccc") +
      geom_point(alpha = 0.85) +
      geom_text(vjust = -1, size = 3, show.legend = FALSE) +
      annotate("text", x =  12, y =  18, label = "Rise",            color = "#aaa", size = 3) +
      annotate("text", x = -12, y =  18, label = "Glove-Side Break", color = "#aaa", size = 3) +
      annotate("text", x =  12, y = -18, label = "Arm-Side Run",    color = "#aaa", size = 3) +
      annotate("text", x = -12, y = -18, label = "Drop",            color = "#aaa", size = 3) +
      scale_color_manual(values = PITCH_COLORS) +
      scale_size_continuous(range = c(3, 10)) +
      labs(
        title = "How Much Each Pitch Moves",
        x = "Horizontal Break (in)", y = "Induced Vert Break (in)",
        color = NULL, size = "Pitches"
      ) +
      theme_seagulls()
    plotly_white(ggplotly(p, tooltip = c("label", "x", "y", "size")))
  })
```

- [ ] **Step 2: Append Chart 6 — Release Point Scatter**

```r
  # ── Chart 6: Release Point Scatter ────────────────────────────────────────
  output$plot_release <- renderPlotly({
    req(nrow(fdata()) > 0)
    p <- ggplot(fdata(), aes(
        x = RelSide, y = RelHeight, color = TaggedPitchType
      )) +
      geom_point(alpha = 0.5, size = 1.5) +
      stat_ellipse(aes(group = TaggedPitchType), linewidth = 0.8) +
      scale_color_manual(values = PITCH_COLORS) +
      labs(
        title    = "Release Point",
        subtitle = "Tighter clusters = more consistent mechanics",
        x = "Horizontal (ft)", y = "Height (ft)", color = NULL
      ) +
      theme_seagulls()
    plotly_white(ggplotly(p, tooltip = c("x", "y", "colour")))
  })
```

- [ ] **Step 3: Append Chart 7 — Pitch Outcome Stacked Bar**

```r
  # ── Chart 7: Pitch Outcome Stacked Bar ────────────────────────────────────
  output$plot_outcomes <- renderPlotly({
    req(nrow(fdata()) > 0)

    outcome_bucket <- function(call) {
      case_when(
        call %in% c("BallCalled","BallinDirt","HitByPitch") ~ "Ball",
        call == "StrikeCalled"                               ~ "Called Strike",
        call %in% c("FoulBallNotFieldable","FoulBallFieldable") ~ "Foul",
        call == "StrikeSwinging"                             ~ "Whiff",
        call == "InPlay"                                     ~ "In Play",
        TRUE                                                 ~ "Other"
      )
    }

    d <- fdata() %>%
      mutate(Outcome = outcome_bucket(PitchCall)) %>%
      group_by(TaggedPitchType) %>%
      mutate(csw = csw_pct(PitchCall)) %>%
      ungroup() %>%
      count(TaggedPitchType, Outcome, csw) %>%
      group_by(TaggedPitchType) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()

    pt_order <- d %>%
      distinct(TaggedPitchType, csw) %>%
      arrange(desc(csw)) %>%
      pull(TaggedPitchType)

    d$TaggedPitchType <- factor(d$TaggedPitchType, levels = pt_order)

    outcome_colors <- c(
      Ball = "#ADB5BD", `Called Strike` = "#457B9D",
      Foul = "#E9C46A", Whiff = "#E63946", `In Play` = "#2DC653"
    )

    p <- ggplot(d, aes(
        x = TaggedPitchType, y = prop, fill = Outcome,
        text = paste0(Outcome, ": ", scales::percent(prop, accuracy = 1))
      )) +
      geom_col(position = "fill") +
      scale_fill_manual(values = outcome_colors) +
      scale_y_continuous(labels = scales::percent_format()) +
      coord_flip() +
      labs(
        title = "What Happened on Each Pitch Type",
        x = NULL, y = "Proportion", fill = NULL
      ) +
      theme_seagulls()
    plotly_white(ggplotly(p, tooltip = "text"))
  })
```

- [ ] **Step 4: Append Chart 8 — Count Heatmap**

```r
  # ── Chart 8: Count Heatmap ────────────────────────────────────────────────
  output$plot_count_heatmap <- renderPlotly({
    req(nrow(fdata()) > 0)

    counts_grid <- expand.grid(
      Balls = 0:3, Strikes = 0:2, stringsAsFactors = FALSE
    )

    d <- fdata() %>%
      group_by(Balls, Strikes) %>%
      summarise(spct = strike_pct(PitchCall), n = n(), .groups = "drop") %>%
      right_join(counts_grid, by = c("Balls", "Strikes")) %>%
      mutate(
        label = if_else(is.na(spct), "—", scales::percent(spct, accuracy = 1)),
        Count = paste0(Balls, "-", Strikes)
      )

    p <- ggplot(d, aes(
        x = factor(Strikes), y = factor(Balls, levels = rev(0:3)),
        fill = spct, label = label
      )) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(size = 4.5, fontface = "bold") +
      scale_fill_gradient2(
        low = "#E63946", mid = "white", high = "#2DC653",
        midpoint = 0.60, na.value = "#f0f0f0",
        labels = scales::percent_format(), name = "Strike%"
      ) +
      labs(
        title = "Strike% by Count",
        x = "Strikes", y = "Balls"
      ) +
      theme_seagulls() +
      theme(panel.grid = element_blank(), legend.position = "right")
    plotly_white(ggplotly(p, tooltip = c("label", "x", "y")))
  })
```

- [ ] **Step 5: Run and verify charts 5–8**

```r
shiny::runApp(".", launch.browser = TRUE)
```

Log in as coach. Pitching tab, scroll down:
- Chart 5: Movement scatter with crosshairs and quadrant labels
- Chart 6: Release point scatter with ellipses
- Chart 7: Horizontal stacked bars ordered by CSW%
- Chart 8: 4×3 heatmap with green/red gradient

- [ ] **Step 6: Commit**

```bash
git add server.R
git commit -m "feat: add pitching charts 5–8 (movement, release, outcomes, count heatmap)"
```

---

## Task 6: Coach hitting tab — Charts 9–11

**Files:**
- Modify: `server.R`

**Interfaces:**
- Consumes: `fdata()`, `theme_seagulls()`, `plotly_white()`, `roster_positions`
- Produces: `output$plot_spray`, `output$plot_ev_la`, `output$table_batters`

- [ ] **Step 1: Append Chart 9 — Spray Chart**

```r
  # ── Chart 9: Spray Chart ──────────────────────────────────────────────────
  output$plot_spray <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      filter(
        !is.na(Direction), !is.na(Distance),
        PlayResult %in% c("Single","Double","Triple","HomeRun","Out")
      ) %>%
      mutate(
        spray_x =  Distance * sin(Direction * pi / 180),
        spray_y =  Distance * cos(Direction * pi / 180)
      )
    req(nrow(d) > 0)

    hit_colors <- c(
      Single = "#2DC653", Double = "#F5C518",
      Triple = "#FF8C00", HomeRun = "#E63946", Out = "#AAAAAA"
    )

    foul_line_len <- 330
    cf_depth      <- 400

    p <- ggplot(d, aes(
        x = spray_x, y = spray_y, color = PlayResult,
        text = paste0(Batter, "<br>", PlayResult, "<br>",
                      round(Distance, 0), " ft @ ", round(Direction, 0), "°")
      )) +
      # Field outline
      annotate("segment", x = 0, xend = -foul_line_len * sin(45 * pi/180),
               y = 0, yend = foul_line_len * cos(45 * pi/180),
               color = "gray70", linewidth = 0.5) +
      annotate("segment", x = 0, xend =  foul_line_len * sin(45 * pi/180),
               y = 0, yend = foul_line_len * cos(45 * pi/180),
               color = "gray70", linewidth = 0.5) +
      annotate("path",
        x = cf_depth * sin(seq(-45, 45, length.out = 100) * pi/180),
        y = cf_depth * cos(seq(-45, 45, length.out = 100) * pi/180),
        color = "gray70", linewidth = 0.5
      ) +
      geom_point(alpha = 0.75, size = 2.5) +
      scale_color_manual(values = hit_colors) +
      coord_fixed(xlim = c(-350, 350), ylim = c(0, 430)) +
      labs(
        title = "Where the Ball Was Hit",
        x = NULL, y = NULL, color = NULL
      ) +
      theme_seagulls() +
      theme(axis.text = element_blank(), panel.grid = element_blank())
    plotly_white(ggplotly(p, tooltip = "text"))
  })
```

- [ ] **Step 2: Append Chart 10 — Exit Velocity vs Launch Angle**

```r
  # ── Chart 10: Exit Velocity vs Launch Angle ────────────────────────────────
  output$plot_ev_la <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      filter(PitchCall == "InPlay", !is.na(ExitSpeed), !is.na(Angle))
    req(nrow(d) > 0)

    result_colors <- c(
      Single = "#2DC653", Double = "#F5C518", Triple = "#FF8C00",
      HomeRun = "#E63946", Out = "#AAAAAA", Error = "#9B2226",
      Undefined = "#CCCCCC"
    )

    p <- ggplot(d, aes(
        x = Angle, y = ExitSpeed, color = PlayResult,
        text = paste0(Batter, "<br>", PlayResult,
                      "<br>EV: ", round(ExitSpeed, 1), " mph",
                      "<br>LA: ", round(Angle, 1), "°")
      )) +
      annotate("rect",
        xmin = 10, xmax = 30, ymin = 95, ymax = Inf,
        fill = "#2DC653", alpha = 0.08
      ) +
      annotate("text", x = 20, y = 117, label = "Barrel Zone",
               color = "#2DC653", size = 3.5, fontface = "bold") +
      geom_point(alpha = 0.7, size = 2) +
      scale_color_manual(values = result_colors) +
      labs(
        title = "Exit Velocity & Launch Angle",
        x = "Launch Angle (°)", y = "Exit Velocity (mph)", color = NULL
      ) +
      theme_seagulls()
    plotly_white(ggplotly(p, tooltip = "text"))
  })
```

- [ ] **Step 3: Append Chart 11 — Batter Leaderboard**

```r
  # ── Chart 11: Batter Leaderboard ──────────────────────────────────────────
  output$table_batters <- renderDT({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      group_by(Batter) %>%
      summarise(
        PA     = n_distinct(paste(Date, Inning, PAofInning)),
        H      = sum(PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm=TRUE),
        BB     = sum(KorBB == "Walk",      na.rm = TRUE),
        K      = sum(KorBB == "Strikeout", na.rm = TRUE),
        Single = sum(PlayResult == "Single",   na.rm = TRUE),
        Double = sum(PlayResult == "Double",   na.rm = TRUE),
        Triple = sum(PlayResult == "Triple",   na.rm = TRUE),
        HR     = sum(PlayResult == "HomeRun",  na.rm = TRUE),
        avg_ev = mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE),
        hh_pct = hard_hit_pct(ExitSpeed[PitchCall == "InPlay"]),
        brl_pct = barrel_pct(
          ExitSpeed[PitchCall == "InPlay"],
          Angle[PitchCall == "InPlay"]
        ),
        .groups = "drop"
      ) %>%
      mutate(
        AB  = PA - BB,
        TB  = Single + 2*Double + 3*Triple + 4*HR,
        AVG = if_else(AB > 0, H / AB, NA_real_),
        OBP = if_else(PA > 0, (H + BB) / PA, NA_real_),
        SLG = if_else(AB > 0, TB / AB, NA_real_),
        `K%`  = K  / PA,
        `BB%` = BB / PA
      ) %>%
      left_join(roster_positions[, c("player_name","position")],
                by = c("Batter" = "player_name")) %>%
      select(Batter, Position = position, PA, AVG, OBP, SLG,
             `K%`, `BB%`, `Avg EV` = avg_ev,
             `Hard Hit%` = hh_pct, `Barrel%` = brl_pct)

    datatable(d,
      options  = list(pageLength = 10, order = list(list(4, "desc"))),
      rownames = FALSE
    ) %>%
      formatRound(c("AVG","OBP","SLG"), digits = 3) %>%
      formatPercentage(c("K%","BB%","Hard Hit%","Barrel%"), digits = 1) %>%
      formatRound("Avg EV", digits = 1) %>%
      formatStyle("Avg EV",
        background = styleInterval(c(82, 92),
          c("#fde8e8", "white", "#e0f5f2"))
      )
  })
```

- [ ] **Step 4: Run and verify charts 9–11**

```r
shiny::runApp(".", launch.browser = TRUE)
```

Switch to Hitting tab as coach:
- Chart 9: Spray chart with field outline and colored hit types
- Chart 10: Scatter with barrel zone highlighted
- Chart 11: Table with Position column and EV color coding

- [ ] **Step 5: Commit**

```bash
git add server.R
git commit -m "feat: add hitting charts 9–11 (spray, EV/LA, batter leaderboard)"
```

---

## Task 7: Coach hitting tab — Charts 12–14 + Insight Box

**Files:**
- Modify: `server.R`

**Interfaces:**
- Consumes: `fdata()`, `theme_seagulls()`, `plotly_white()`, all metric functions
- Produces: `output$plot_swing_zones`, `output$plot_hit_types`, `output$plot_pitch_vuln`, `output$insights`

- [ ] **Step 1: Append Chart 12 — Swing Decision Heatmap**

```r
  # ── Chart 12: Swing Decision Heatmap ──────────────────────────────────────
  output$plot_swing_zones <- renderPlotly({
    req(nrow(fdata()) > 0)

    zone_w <- (SZ_RIGHT - SZ_LEFT) / 3
    zone_h <- (SZ_TOP   - SZ_BOT)  / 3

    d <- fdata() %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        zone_col = cut(PlateLocSide,
          breaks = c(-Inf, SZ_LEFT + zone_w, SZ_LEFT + 2*zone_w, Inf),
          labels = c("Left","Middle","Right")
        ),
        zone_row = cut(PlateLocHeight,
          breaks = c(-Inf, SZ_BOT + zone_h, SZ_BOT + 2*zone_h, Inf),
          labels = c("Low","Mid","High")
        ),
        swing = PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable",
                                  "FoulBallFieldable","InPlay")
      ) %>%
      filter(!is.na(zone_col), !is.na(zone_row)) %>%
      group_by(zone_col, zone_row) %>%
      summarise(
        swing_pct = mean(swing), n = n(), .groups = "drop"
      )

    p <- ggplot(d, aes(
        x = zone_col, y = zone_row, fill = swing_pct,
        label = scales::percent(swing_pct, accuracy = 1)
      )) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(size = 5, fontface = "bold") +
      scale_fill_gradient2(
        low = "#457B9D", mid = "white", high = "#E63946",
        midpoint = 0.5, labels = scales::percent_format(), name = "Swing%"
      ) +
      scale_x_discrete(limits = c("Left","Middle","Right")) +
      scale_y_discrete(limits = c("Low","Mid","High")) +
      labs(
        title    = "Swing Rates by Zone",
        subtitle = "Do hitters swing at the right pitches?",
        x = NULL, y = NULL
      ) +
      theme_seagulls() + theme(panel.grid = element_blank())
    plotly_white(ggplotly(p, tooltip = c("x","y","label")))
  })
```

- [ ] **Step 2: Append Chart 13 — Hit Type Distribution**

```r
  # ── Chart 13: Hit Type Distribution ───────────────────────────────────────
  output$plot_hit_types <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>% filter(TaggedHitType %in% BIP_TYPES)
    req(nrow(d) > 0)

    bip_colors <- c(
      GroundBall = "#8B4513", FlyBall  = "#457B9D",
      LineDrive  = "#2DC653", Popup    = "#AAAAAA"
    )

    p <- ggplot(d, aes(x = Batter, fill = TaggedHitType)) +
      geom_bar(position = "fill") +
      scale_fill_manual(values = bip_colors) +
      scale_y_continuous(labels = scales::percent_format()) +
      coord_flip() +
      labs(
        title = "Batted Ball Type by Hitter",
        x = NULL, y = "Proportion", fill = NULL
      ) +
      theme_seagulls()
    plotly_white(ggplotly(p, tooltip = c("x","fill","count")))
  })
```

- [ ] **Step 3: Append Chart 14 — Pitch Vulnerability Heatmap**

```r
  # ── Chart 14: Pitch Vulnerability Heatmap ─────────────────────────────────
  output$plot_pitch_vuln <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      group_by(Batter, TaggedPitchType) %>%
      summarise(
        wp = whiff_pct(PitchCall),
        n  = n(),
        .groups = "drop"
      ) %>%
      mutate(wp = if_else(n < 5, NA_real_, wp))

    p <- ggplot(d, aes(
        x = TaggedPitchType, y = Batter, fill = wp,
        label = if_else(is.na(wp), "—", scales::percent(wp, accuracy = 1))
      )) +
      geom_tile(color = "white", linewidth = 0.4) +
      geom_text(size = 3) +
      scale_fill_gradient2(
        low = "#2DC653", mid = "white", high = "#E63946",
        midpoint = 0.25, na.value = "#f0f0f0",
        labels = scales::percent_format(), name = "Whiff%"
      ) +
      labs(
        title = "Batter Whiff Rate vs. Each Pitch Type",
        x = NULL, y = NULL
      ) +
      theme_seagulls() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            panel.grid  = element_blank())
    plotly_white(ggplotly(p, tooltip = c("x","y","label")))
  })
```

- [ ] **Step 4: Append Insight Box**

```r
  # ── Insight Box ───────────────────────────────────────────────────────────
  output$insights <- renderUI({
    req(nrow(fdata()) > 0)
    d <- fdata()

    spct   <- strike_pct(d$PitchCall)
    wpct   <- whiff_pct(d$PitchCall)
    cswpct <- csw_pct(d$PitchCall)
    cpct   <- chase_pct(d$PlateLocSide, d$PlateLocHeight, d$PitchCall)

    best_whiff <- d %>%
      group_by(TaggedPitchType) %>%
      summarise(wp = whiff_pct(PitchCall), n = n(), .groups = "drop") %>%
      filter(n >= 10) %>%
      slice_max(wp, n = 1, with_ties = FALSE)

    fmt <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 1)

    tagList(
      tags$div(
        style = "background:#f8f9fa; border-radius:8px; padding:12px;
                 border:1px solid #e0e0e0; font-size:13px;",
        tags$strong("Key Metrics", style = "display:block; margin-bottom:8px;
                     font-size:14px; color:#0a1628;"),
        tags$div(paste0("Strike%: ",  fmt(spct))),
        tags$div(paste0("Whiff%: ",   fmt(wpct))),
        tags$div(paste0("CSW%: ",     fmt(cswpct))),
        tags$div(paste0("Chase%: ",   fmt(cpct))),
        if (nrow(best_whiff) > 0)
          tags$div(
            style = "margin-top:8px; padding-top:8px; border-top:1px solid #ddd;",
            tags$strong("Best Whiff Pitch: "),
            paste0(best_whiff$TaggedPitchType, " (", fmt(best_whiff$wp), ")")
          )
      )
    )
  })
```

- [ ] **Step 5: Run and verify charts 12–14 + insight box**

```r
shiny::runApp(".", launch.browser = TRUE)
```

Hitting tab as coach:
- Chart 12: 3×3 swing rate heatmap
- Chart 13: Horizontal stacked bars per batter
- Chart 14: Batter × pitch type heatmap; cells with <5 pitches show "—"
- Sidebar: Insight box shows Strike%, Whiff%, CSW%, Chase%, best whiff pitch

- [ ] **Step 6: Commit**

```bash
git add server.R
git commit -m "feat: add hitting charts 12–14 and insight box"
```

---

## Task 8: Player view — layout + game browser

**Files:**
- Modify: `server.R`

**Interfaces:**
- Consumes: `user_player_name()`, `user_player_type()`, `user_display_name()` from Task 3
- Consumes: `player_fdata_base()` from Task 3
- Produces: `selected_game()` reactive — a single `Date` value (consumed by Tasks 9–10)
- Produces: `player_fdata()` reactive — rows for logged-in player on `selected_game()` (consumed by Tasks 9–10)
- Produces: `output$player_ui` renderUI

- [ ] **Step 1: Append player game browser reactives to `server.R`**

```r
  # ── Player: game list and selection ───────────────────────────────────────
  player_games <- reactive({
    req(user_role() == "player")
    ptype <- user_player_type()
    col   <- if (ptype == "pitcher") "Pitcher" else "Batter"
    dates <- player_fdata_base() %>%
      pull(Date) %>% unique() %>% sort(decreasing = TRUE)
    dates
  })

  game_index <- reactiveVal(1L)

  observeEvent(player_games(), { game_index(1L) }, ignoreNULL = TRUE)

  observeEvent(input$prev_game, {
    g <- player_games()
    if (length(g) > 0)
      game_index(min(game_index() + 1L, length(g)))
  })

  observeEvent(input$next_game, {
    if (game_index() > 1L)
      game_index(game_index() - 1L)
  })

  selected_game <- reactive({
    g <- player_games()
    req(length(g) > 0)
    g[[game_index()]]
  })

  player_fdata <- reactive({
    req(user_role() == "player")
    ptype <- user_player_type()
    col   <- if (ptype == "pitcher") "Pitcher" else "Batter"
    player_fdata_base() %>%
      filter(Date == selected_game())
  })
```

- [ ] **Step 2: Append player_ui renderUI**

```r
  # ── Player: top-level UI ─────────────────────────────────────────────────
  output$player_ui <- renderUI({
    req(user_role() == "player")
    name  <- user_display_name()
    ptype <- user_player_type()

    pos_row <- roster_positions %>%
      filter(player_name == user_player_name())
    jersey   <- if (nrow(pos_row) > 0) pos_row$jersey[1]   else ""
    position <- if (nrow(pos_row) > 0) pos_row$position[1] else ""

    n_games <- length(player_games())

    tagList(
      # Header
      div(
        style = "display:flex; justify-content:space-between; align-items:center;
                 margin-bottom:16px; padding-bottom:12px; border-bottom:1px solid #eee;",
        div(
          tags$h5(name, style = "margin:0; color:#0a1628;"),
          tags$small(paste0("#", jersey, " · ", position), style = "color:#666;")
        ),
        actionButton("logout", "Log Out", class = "btn-sm btn-outline-secondary")
      ),
      # Game browser
      if (n_games == 0) {
        div("No games recorded yet.", style = "color:#888; margin:24px 0;")
      } else {
        tagList(
          div(
            class = "game-nav",
            actionButton("prev_game", "← Prev Game",
              class = "btn-sm btn-outline-secondary",
              disabled = game_index() >= n_games
            ),
            span(
              class = "game-label",
              format(selected_game(), "%B %d, %Y"),
              style = "color:#0a1628;"
            ),
            actionButton("next_game", "Next Game →",
              class = "btn-sm btn-outline-secondary",
              disabled = game_index() <= 1
            )
          ),
          # Section(s) based on player type
          if (ptype %in% c("pitcher","two-way")) uiOutput("player_pitcher_section"),
          if (ptype == "two-way") hr(),
          if (ptype %in% c("hitter","two-way"))  uiOutput("player_hitter_section")
        )
      }
    )
  })
```

- [ ] **Step 3: Verify player login shows layout**

```r
shiny::runApp(".", launch.browser = TRUE)
```

Log in as a pitcher, e.g. `declan_mendel` / `6`. Should see:
- Header with name, jersey, position
- Game navigation bar (or "No games recorded yet" if no data for this player)
- No chart content yet (Tasks 9–10 add that)

Log in as a hitter, e.g. `bryce_brooks` / `1`. Same structure.

- [ ] **Step 4: Commit**

```bash
git add server.R
git commit -m "feat: add player view layout and game browser reactives"
```

---

## Task 9: Player view — pitcher scorecard + charts

**Files:**
- Modify: `server.R`

**Interfaces:**
- Consumes: `player_fdata()` from Task 8, all metric functions, `PITCH_COLORS`, `theme_seagulls()`, `plotly_white()`
- Produces: `output$player_pitcher_section` (renderUI with scorecard + 4 charts)
- Produces: `output$player_zone`, `output$player_arsenal`, `output$player_release`, `output$player_outcomes`

- [ ] **Step 1: Append pitcher scorecard helper function**

Add this helper function near the top of `server.R` (before `server <- function`):

```r
# Stat tile helper for player scorecard
stat_tile <- function(label, value_str, css_class = "tile-neutral") {
  div(
    class = "value-tile",
    div(class = "tile-label", label),
    div(class = paste("tile-value", css_class), value_str)
  )
}

tile_class <- function(val, hi_thr, lo_thr, hi_good = TRUE) {
  if (is.na(val)) return("tile-neutral")
  if (hi_good) {
    if (val >= hi_thr) "tile-teal"
    else if (val <= lo_thr) "tile-amber"
    else "tile-neutral"
  } else {
    if (val <= hi_thr) "tile-teal"
    else if (val >= lo_thr) "tile-amber"
    else "tile-neutral"
  }
}
```

- [ ] **Step 2: Append pitcher section renderUI**

```r
  # ── Player: pitcher section ───────────────────────────────────────────────
  output$player_pitcher_section <- renderUI({
    d <- player_fdata()

    spct  <- strike_pct(d$PitchCall)
    wpct  <- whiff_pct(d$PitchCall)
    cswp  <- csw_pct(d$PitchCall)
    chsp  <- chase_pct(d$PlateLocSide, d$PlateLocHeight, d$PitchCall)

    fmt <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 1)

    tagList(
      tags$h6("Pitching", style = "color:#0a1628; font-weight:600; margin:12px 0 8px;"),
      # Scorecard tiles
      layout_columns(
        col_widths = breakpoints(sm = 6, md = 3),
        stat_tile("Strike%", fmt(spct), tile_class(spct, 0.65, 0.54)),
        stat_tile("Whiff%",  fmt(wpct), tile_class(wpct, 0.30, 0.19)),
        stat_tile("CSW%",    fmt(cswp), tile_class(cswp, 0.28, 0.20)),
        stat_tile("Chase%",  fmt(chsp), tile_class(chsp, 0.30, 0.00))
      ),
      # Charts
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6),
        plotlyOutput("player_zone",     height = "340px"),
        plotlyOutput("player_arsenal",  height = "340px"),
        plotlyOutput("player_release",  height = "340px"),
        plotlyOutput("player_outcomes", height = "340px")
      )
    )
  })
```

- [ ] **Step 3: Append 4 pitcher chart renders**

```r
  output$player_zone <- renderPlotly({
    d <- player_fdata()
    req(nrow(d) > 0)
    p <- ggplot(d, aes(
        x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType,
        text = paste0(TaggedPitchType, "<br>", round(RelSpeed,1), " mph<br>", PitchCall)
      )) +
      geom_point(alpha = 0.6, size = 2.5) +
      annotate("rect", xmin=SZ_LEFT, xmax=SZ_RIGHT, ymin=SZ_BOT, ymax=SZ_TOP,
               fill=NA, color="black", linewidth=0.8) +
      scale_color_manual(values = PITCH_COLORS, drop = FALSE) +
      scale_x_continuous(limits = c(-2.5, 2.5)) +
      scale_y_continuous(limits = c(0, 5)) +
      labs(title="Pitch Location", x="Horizontal (ft)", y="Height (ft)", color=NULL) +
      theme_seagulls() + coord_fixed()
    plotly_white(ggplotly(p, tooltip = "text"))
  })

  output$player_arsenal <- renderPlotly({
    d <- player_fdata()
    req(nrow(d) > 0)
    ds <- d %>%
      group_by(TaggedPitchType) %>%
      summarise(n=n(), avg_spd=round(mean(RelSpeed,na.rm=TRUE),1),
                avg_spin=round(mean(SpinRate,na.rm=TRUE),0), .groups="drop") %>%
      mutate(pct = n / sum(n))
    plot_ly(ds,
      labels=~TaggedPitchType, values=~n, type="pie", hole=0.5,
      marker=list(colors=unname(PITCH_COLORS[ds$TaggedPitchType])),
      text=~paste0(TaggedPitchType,"<br>",scales::percent(pct,1),"<br>",avg_spd," mph"),
      hoverinfo="text", textinfo="label+percent"
    ) %>% layout(title=list(text="Pitch Mix"), showlegend=FALSE,
                  paper_bgcolor="white", plot_bgcolor="white",
                  font=list(color="#0a1628"))
  })

  output$player_release <- renderPlotly({
    d <- player_fdata()
    req(nrow(d) > 0)
    p <- ggplot(d, aes(x=RelSide, y=RelHeight, color=TaggedPitchType)) +
      geom_point(alpha=0.5, size=1.5) +
      stat_ellipse(aes(group=TaggedPitchType), linewidth=0.8) +
      scale_color_manual(values=PITCH_COLORS) +
      labs(title="Release Point",
           subtitle="Tighter clusters = more consistent",
           x="Horizontal (ft)", y="Height (ft)", color=NULL) +
      theme_seagulls()
    plotly_white(ggplotly(p, tooltip=c("x","y","colour")))
  })

  output$player_outcomes <- renderPlotly({
    d <- player_fdata()
    req(nrow(d) > 0)
    outcome_bucket <- function(call) {
      case_when(
        call %in% c("BallCalled","BallinDirt","HitByPitch") ~ "Ball",
        call == "StrikeCalled"  ~ "Called Strike",
        call %in% c("FoulBallNotFieldable","FoulBallFieldable") ~ "Foul",
        call == "StrikeSwinging" ~ "Whiff",
        call == "InPlay"         ~ "In Play",
        TRUE                     ~ "Other"
      )
    }
    outcome_colors <- c(Ball="#ADB5BD",`Called Strike`="#457B9D",
                         Foul="#E9C46A",Whiff="#E63946",`In Play`="#2DC653")
    ds <- d %>%
      mutate(Outcome=outcome_bucket(PitchCall), csw=csw_pct(PitchCall)) %>%
      count(TaggedPitchType, Outcome) %>%
      group_by(TaggedPitchType) %>%
      mutate(prop=n/sum(n)) %>% ungroup()
    p <- ggplot(ds, aes(x=TaggedPitchType, y=prop, fill=Outcome,
        text=paste0(Outcome,": ",scales::percent(prop,1)))) +
      geom_col(position="fill") +
      scale_fill_manual(values=outcome_colors) +
      scale_y_continuous(labels=scales::percent_format()) +
      coord_flip() +
      labs(title="Pitch Outcomes", x=NULL, y=NULL, fill=NULL) +
      theme_seagulls()
    plotly_white(ggplotly(p, tooltip="text"))
  })
```

- [ ] **Step 4: Verify pitcher player view**

```r
shiny::runApp(".", launch.browser = TRUE)
```

Log in as `declan_mendel` / `6`. Verify:
- Header shows "Declan Mendel · #6 · RHP"
- Game browser shows most recent game date
- Pitching scorecard: 4 tiles (Strike%, Whiff%, CSW%, Chase%) with teal/amber color coding
- 4 charts render: zone map, arsenal donut, release point, outcomes bar

- [ ] **Step 5: Commit**

```bash
git add server.R
git commit -m "feat: add player pitcher scorecard and 4 pitching charts"
```

---

## Task 10: Player view — hitter scorecard + charts

**Files:**
- Modify: `server.R`

**Interfaces:**
- Consumes: `player_fdata()` from Task 8, metric functions, `BIP_TYPES`, `theme_seagulls()`, `plotly_white()`
- Produces: `output$player_hitter_section` (renderUI)
- Produces: `output$player_spray`, `output$player_swing_zones`, `output$player_hit_types`

- [ ] **Step 1: Append hitter section renderUI**

```r
  # ── Player: hitter section ────────────────────────────────────────────────
  output$player_hitter_section <- renderUI({
    d <- player_fdata()

    d_ip <- d %>% filter(PitchCall == "InPlay", !is.na(ExitSpeed))
    avg_ev <- mean(d_ip$ExitSpeed, na.rm = TRUE)
    hh     <- hard_hit_pct(d_ip$ExitSpeed)

    # Zone swing%
    d_zone <- d %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    in_zone <- d_zone$PlateLocSide >= SZ_LEFT & d_zone$PlateLocSide <= SZ_RIGHT &
               d_zone$PlateLocHeight >= SZ_BOT  & d_zone$PlateLocHeight <= SZ_TOP
    swings <- d_zone$PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable",
                                       "FoulBallFieldable","InPlay")
    swing_zone <- if (sum(in_zone) > 0) sum(in_zone & swings) / sum(in_zone) else NA_real_

    ooz <- !in_zone
    chase <- if (sum(ooz) > 0) sum(ooz & swings) / sum(ooz) else NA_real_

    fmt_ev  <- function(x) if (is.na(x)) "—" else paste0(round(x,1), " mph")
    fmt_pct <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy=1)

    tagList(
      tags$h6("Hitting", style = "color:#0a1628; font-weight:600; margin:12px 0 8px;"),
      layout_columns(
        col_widths = breakpoints(sm = 6, md = 3),
        stat_tile("Avg Exit Velo", fmt_ev(avg_ev),
                  tile_class(avg_ev, 92, 82)),
        stat_tile("Hard Hit%", fmt_pct(hh),
                  tile_class(hh, 0.40, 0.25)),
        stat_tile("Zone Swing%", fmt_pct(swing_zone),
                  tile_class(swing_zone, 0.70, 0.00)),
        stat_tile("Chase%", fmt_pct(chase),
                  tile_class(chase, 0.25, 0.35, hi_good = FALSE))
      ),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 4),
        plotlyOutput("player_spray",       height = "360px"),
        plotlyOutput("player_swing_zones", height = "360px"),
        plotlyOutput("player_hit_types",   height = "360px")
      )
    )
  })
```

- [ ] **Step 2: Append 3 hitter chart renders**

```r
  output$player_spray <- renderPlotly({
    d <- player_fdata() %>%
      filter(!is.na(Direction), !is.na(Distance),
             PlayResult %in% c("Single","Double","Triple","HomeRun","Out")) %>%
      mutate(spray_x = Distance * sin(Direction * pi/180),
             spray_y = Distance * cos(Direction * pi/180))
    req(nrow(d) > 0)
    hit_colors <- c(Single="#2DC653",Double="#F5C518",
                    Triple="#FF8C00",HomeRun="#E63946",Out="#AAAAAA")
    p <- ggplot(d, aes(x=spray_x, y=spray_y, color=PlayResult,
        text=paste0(PlayResult,"<br>",round(Distance,0)," ft"))) +
      annotate("segment",x=0,xend=-233,y=0,yend=233,color="gray70",linewidth=0.5) +
      annotate("segment",x=0,xend=233, y=0,yend=233,color="gray70",linewidth=0.5) +
      annotate("path",
        x=400*sin(seq(-45,45,length.out=100)*pi/180),
        y=400*cos(seq(-45,45,length.out=100)*pi/180),
        color="gray70",linewidth=0.5) +
      geom_point(alpha=0.8,size=3) +
      scale_color_manual(values=hit_colors) +
      coord_fixed(xlim=c(-350,350),ylim=c(0,430)) +
      labs(title="Where the Ball Was Hit",x=NULL,y=NULL,color=NULL) +
      theme_seagulls() +
      theme(axis.text=element_blank(),panel.grid=element_blank())
    plotly_white(ggplotly(p, tooltip="text"))
  })

  output$player_swing_zones <- renderPlotly({
    d <- player_fdata()
    req(nrow(d) > 0)
    zone_w <- (SZ_RIGHT - SZ_LEFT) / 3
    zone_h <- (SZ_TOP   - SZ_BOT)  / 3
    ds <- d %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        zone_col = cut(PlateLocSide,
          breaks=c(-Inf,SZ_LEFT+zone_w,SZ_LEFT+2*zone_w,Inf),
          labels=c("Left","Middle","Right")),
        zone_row = cut(PlateLocHeight,
          breaks=c(-Inf,SZ_BOT+zone_h,SZ_BOT+2*zone_h,Inf),
          labels=c("Low","Mid","High")),
        swing = PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable",
                                  "FoulBallFieldable","InPlay")
      ) %>%
      filter(!is.na(zone_col),!is.na(zone_row)) %>%
      group_by(zone_col,zone_row) %>%
      summarise(swing_pct=mean(swing), .groups="drop")
    p <- ggplot(ds, aes(x=zone_col, y=zone_row, fill=swing_pct,
        label=scales::percent(swing_pct,1))) +
      geom_tile(color="white",linewidth=0.8) +
      geom_text(size=5,fontface="bold") +
      scale_fill_gradient2(low="#457B9D",mid="white",high="#E63946",
        midpoint=0.5,labels=scales::percent_format(),name="Swing%") +
      scale_x_discrete(limits=c("Left","Middle","Right")) +
      scale_y_discrete(limits=c("Low","Mid","High")) +
      labs(title="Swing Rates by Zone",x=NULL,y=NULL) +
      theme_seagulls() + theme(panel.grid=element_blank())
    plotly_white(ggplotly(p, tooltip=c("x","y","label")))
  })

  output$player_hit_types <- renderPlotly({
    d <- player_fdata() %>% filter(TaggedHitType %in% BIP_TYPES)
    req(nrow(d) > 0)
    bip_colors <- c(GroundBall="#8B4513",FlyBall="#457B9D",
                    LineDrive="#2DC653",Popup="#AAAAAA")
    ds <- d %>% count(TaggedHitType) %>% mutate(pct=n/sum(n))
    p <- ggplot(ds, aes(x="",y=pct,fill=TaggedHitType,
        text=paste0(TaggedHitType,": ",scales::percent(pct,1)))) +
      geom_col(width=1) +
      scale_fill_manual(values=bip_colors) +
      scale_y_continuous(labels=scales::percent_format()) +
      coord_flip() +
      labs(title="Batted Ball Types",x=NULL,y="Proportion",fill=NULL) +
      theme_seagulls() + theme(axis.text.y=element_blank())
    plotly_white(ggplotly(p, tooltip="text"))
  })
```

- [ ] **Step 3: Verify hitter player view**

```r
shiny::runApp(".", launch.browser = TRUE)
```

Log in as `bryce_brooks` / `1`. Verify:
- Header shows "Bryce Brooks · #1 · INF"
- Hitter scorecard: Avg EV, Hard Hit%, Zone Swing%, Chase% tiles with color coding
- 3 charts: spray chart, swing zone heatmap, batted ball type breakdown
- No AVG, OBP, SLG, K%, BB% anywhere on the page

- [ ] **Step 4: Commit**

```bash
git add server.R
git commit -m "feat: add player hitter scorecard and 3 hitting charts"
```

---

## Task 11: Final integration + smoke test

**Files:**
- Modify: `server.R` (player logout), `ui.R` (verify outputs match renders), `global.R` (add shinymanager to library calls if missing)

**Interfaces:**
- Consumes: everything built in Tasks 1–10
- Produces: a fully working app, verified end-to-end

- [ ] **Step 1: Confirm shinymanager is in `global.R`**

Open `global.R`. If `library(shinymanager)` is not already there, add it after `library(DT)`:

```r
library(shinymanager)
```

Also add `source("roster.R")` at the end of `global.R`:

```r
source("roster.R")
```

- [ ] **Step 2: Confirm player logout works**

`server.R` already has `observeEvent(input$logout, { session$reload() })` from Task 3. This covers both coach and player logout since both views render a button with `inputId = "logout"`. Verify it's present once (not duplicated).

- [ ] **Step 3: Full smoke test — coach flow**

```r
shiny::runApp(".", launch.browser = TRUE)
```

1. Log in as `coach_cascone` / `seagulls2026`
2. Confirm sidebar shows "Logged in as: Cage Cascone (Coach)" + Log Out button
3. Pitching tab: all 8 charts render, leaderboard has Position column
4. Switch to Hitting tab: all 6 charts render, batter leaderboard has Position column
5. Sidebar insight box shows metrics
6. Change view_mode to Hitting: player dropdown updates to batters
7. Select a single player: all charts filter to that player
8. Click Log Out: returns to login screen

- [ ] **Step 4: Full smoke test — pitcher player flow**

1. Log in as `theodore_tsouras` / `12`
2. Confirm header: "Theodore Tsouras · #12 · LHP"
3. Game browser shows dates; Prev/Next navigate
4. Pitcher scorecard tiles change when game changes
5. 4 pitcher charts render and update with game selection
6. No hitting section visible
7. Log Out works

- [ ] **Step 5: Full smoke test — hitter player flow**

1. Log in as `davis_germann` / `9`
2. Confirm header: "Davis Germann · #9 · OF"
3. Hitter scorecard visible, no pitcher section
4. 3 hitter charts render
5. Spray chart, swing zones, hit types all update with game selection

- [ ] **Step 6: Run unit tests one final time**

```r
testthat::test_dir("tests/testthat")
```

Expected: All 11 tests pass.

- [ ] **Step 7: Final commit**

```bash
git add global.R server.R
git commit -m "feat: finalize app integration — shinymanager auth, coach and player views complete"
```
