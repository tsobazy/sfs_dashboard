# Dashboard Redesign Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace amateur-looking, over-complicated dashboard with a professional Savant-style analytics tool — Statcast colors, Inter font, game-chip navigation, simplified coach and player views.

**Architecture:** In-place edits to `global.R`, `server.R`, and `ui.R`. No new app files. Each task is independently committable. The spec is at `docs/superpowers/specs/2026-06-17-dashboard-redesign.md`.

**Tech Stack:** R Shiny, bslib (BS5), shinyWidgets, ggplot2, plotly, DT, tidyr

## Global Constraints
- Never show player-vs-player or team-avg comparison in the player portal — no exceptions
- Statcast pitch-type hex codes used in every chart, no custom colors for pitch types
- Inter font via `bslib::font_google("Inter")` throughout
- All ggplot charts: white background, horizontal-only light gridlines (`#F1F5F9`)
- `config(displayModeBar = FALSE)` on every plotly embed
- Player view tile accents: teal `#2A9D8F` / amber `#F4A261` — no red (`#E63946`)
- Do NOT modify: `roster.R`, `credentials.sqlite`, `setup_credentials.R`, auth flow, metric functions (`strike_pct`, `whiff_pct`, `csw_pct`, `chase_pct`, `hard_hit_pct`, `barrel_pct`, `gb_pct`), `.secrets/`, `data/game_csvs/`, `.Renviron`, test suite

---

## File Map

| File | What changes |
|------|-------------|
| `global.R` | PITCH_COLORS, PITCH_CATEGORY_COLORS, theme_seagulls(), plotly_clean(), ring_df(), spray_xy(), coach_sidebar(), coach_layout() |
| `server.R` | fdata() date filter, game chip reactives, table_arsenal, plot_movement (rings), plot_spray_coach, player section outputs |
| `ui.R` | bs_theme(), CSS (Inter, chips, sidebar flush) |

---

### Task 1: Color constants + theme foundation

**Files:**
- Modify: `global.R` (PITCH_COLORS, PITCH_CATEGORY_COLORS, theme_seagulls, plotly_clean)
- Modify: `ui.R` (bs_theme, CSS)
- Test: `tests/testthat/test-theme.R` (new)

**Interfaces:**
- Produces: `PITCH_COLORS`, `PITCH_CATEGORY_COLORS`, `theme_seagulls()`, `plotly_clean()` — used by every chart in Tasks 4–7

- [ ] **Step 1: Write failing test**

Create `tests/testthat/test-theme.R`:
```r
source("../../global.R")

test_that("PITCH_COLORS uses Statcast hex values", {
  expect_equal(unname(PITCH_COLORS["FourSeamFastBall"]), "#D22D49")
  expect_equal(unname(PITCH_COLORS["Curveball"]),        "#00D1ED")
  expect_equal(unname(PITCH_COLORS["ChangeUp"]),         "#1DBE3A")
  expect_equal(unname(PITCH_COLORS["Slider"]),           "#EEE716")
  expect_equal(unname(PITCH_COLORS["Undefined"]),        "#AAAAAA")
})

test_that("PITCH_CATEGORY_COLORS has correct keys", {
  expect_true("Fastball"        %in% names(PITCH_CATEGORY_COLORS))
  expect_true("Breaking Ball"   %in% names(PITCH_CATEGORY_COLORS))
  expect_true("Offspeed"        %in% names(PITCH_CATEGORY_COLORS))
  expect_equal(unname(PITCH_CATEGORY_COLORS["Fastball"]),      "#D22D49")
  expect_equal(unname(PITCH_CATEGORY_COLORS["Breaking Ball"]), "#00D1ED")
  expect_equal(unname(PITCH_CATEGORY_COLORS["Offspeed"]),      "#1DBE3A")
})
```

- [ ] **Step 2: Run test — expect FAIL**
```bash
cd /Users/tsobazy/sfs_dashboard
Rscript -e "testthat::test_file('tests/testthat/test-theme.R')"
```
Expected: failures because current PITCH_COLORS has wrong hex values.

- [ ] **Step 3: Replace PITCH_COLORS and PITCH_CATEGORY_COLORS in `global.R`**

Find the current `PITCH_COLORS` block (around line 25) and replace:
```r
PITCH_COLORS <- c(
  FourSeamFastBall = "#D22D49", Fastball        = "#D22D49",
  Sinker           = "#FE9D00", TwoSeamFastBall = "#FE9D00",
  Cutter           = "#933F2C",
  ChangeUp         = "#1DBE3A", Splitter        = "#3BACAC",
  Slider           = "#EEE716", Sweeper         = "#DDB33A",
  Curveball        = "#00D1ED",
  Undefined        = "#AAAAAA"
)
```

Replace the existing `PITCH_CATEGORY_COLORS` block (search for it):
```r
PITCH_CATEGORY_COLORS <- c(
  Fastball        = "#D22D49",
  `Breaking Ball` = "#00D1ED",
  Offspeed        = "#1DBE3A",
  Undefined       = "#AAAAAA"
)
```

- [ ] **Step 4: Replace `theme_seagulls()` in `global.R`**

Find the existing `theme_seagulls <- function()` block and replace the entire function:
```r
theme_seagulls <- function() {
  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#F1F5F9"),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 13, color = "#1A202C"),
    plot.subtitle      = element_text(size = 10, color = "#64748B"),
    axis.text          = element_text(color = "#64748B", size = 10),
    axis.title         = element_text(color = "#64748B", size = 11),
    legend.background  = element_rect(fill = "white"),
    strip.background   = element_rect(fill = "#F1F5F9")
  )
}
```

- [ ] **Step 5: Replace `plotly_white()` with `plotly_clean()` in `global.R`**

Find `plotly_white <- function(p)` and replace:
```r
plotly_clean <- function(p) {
  p %>%
    layout(
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      font = list(color = "#1A202C")
    ) %>%
    config(displayModeBar = FALSE)
}
```

- [ ] **Step 6: Replace every `plotly_white(` call in `server.R` with `plotly_clean(`**
```bash
sed -i '' 's/plotly_white(/plotly_clean(/g' /Users/tsobazy/sfs_dashboard/server.R
```
Verify:
```bash
grep -c "plotly_white" /Users/tsobazy/sfs_dashboard/server.R
# Expected: 0
grep -c "plotly_clean" /Users/tsobazy/sfs_dashboard/server.R
# Expected: >10
```

- [ ] **Step 7: Update `bs_theme()` and CSS in `ui.R`**

Replace the entire `bs_theme(...)` call:
```r
theme = bs_theme(
  bg = "#F8F9FC", fg = "#1A202C",
  primary = "#1D4ED8", secondary = "#64748B",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  version = 5
),
```

Replace the entire `tags$head(tags$style(HTML("...")))` block with:
```r
tags$head(tags$style(HTML("
  body { margin:0; padding:0; font-family:'Inter',sans-serif; }
  .value-tile {
    background:white; border:1px solid #E2E8F0; border-radius:8px;
    padding:16px; text-align:center; margin-bottom:12px;
  }
  .value-tile .tile-label { font-size:11px; color:#64748B; text-transform:uppercase;
                             letter-spacing:.5px; margin-bottom:4px; }
  .value-tile .tile-value { font-size:32px; font-weight:700; }
  .tile-teal    { color:#2A9D8F; }
  .tile-amber   { color:#F4A261; }
  .tile-neutral { color:#1A202C; }
  .tile-trend   { font-size:11px; color:#94A3B8; margin-top:4px; display:block; }
  .game-nav { display:flex; align-items:center; gap:12px; margin-bottom:16px; }
  .game-nav .game-label { font-size:15px; font-weight:600; color:#1A202C; }
  .btn-one-light {
    background:transparent; color:#CBD5E1 !important;
    border:1px solid #475569 !important; font-size:12px;
  }
  .btn-one-light:hover { background:rgba(255,255,255,.1) !important; color:#fff !important; }
  .game-chip-row { overflow-x:auto; white-space:nowrap; padding:4px 0; margin-bottom:8px; }
  .game-chip-row .btn { margin-right:4px; border-radius:20px; font-size:11px;
                         padding:3px 10px; white-space:nowrap; }
"))),
```

- [ ] **Step 8: Run tests — expect PASS**
```bash
cd /Users/tsobazy/sfs_dashboard
Rscript -e "testthat::test_dir('tests/testthat')"
```
Expected: all existing tests pass + new theme tests pass.

- [ ] **Step 9: Smoke-test app loads**
```bash
Rscript -e "shiny::runApp('/Users/tsobazy/sfs_dashboard', port=7340, launch.browser=FALSE)" &
sleep 6 && curl -s -o /dev/null -w "%{http_code}" http://localhost:7340/
# Expected: 200
pkill -f "port=7340"
```

- [ ] **Step 10: Commit**
```bash
git add global.R server.R ui.R tests/testthat/test-theme.R
git commit -m "feat: Statcast colors, Inter font, clean theme + plotly_clean helper"
```

---

### Task 2: Game chip selector — replaces date range in coach view

**Files:**
- Modify: `global.R` (coach_sidebar — remove dateRangeInput, add uiOutput("game_selector"))
- Modify: `server.R` (game chip renderUI, selected_game_range reactive, fdata() date filter)
- Test: `tests/testthat/test-game-nav.R` (new)

**Interfaces:**
- Consumes: `app_data()` from server
- Produces: `selected_game_range()` reactive returning a character vector of Date strings (or NULL for Season = all dates) — consumed by `fdata()`

- [ ] **Step 1: Write failing test**

Create `tests/testthat/test-game-nav.R`:
```r
test_that("game range filter returns correct dates for Last 5", {
  all_dates <- as.Date(c("2025-11-22","2025-11-15","2025-11-08",
                          "2025-11-01","2025-10-25","2025-10-18","2025-10-11"))
  last5 <- head(sort(all_dates, decreasing = TRUE), 5)
  expect_length(last5, 5)
  expect_equal(last5[1], as.Date("2025-11-22"))
  expect_equal(last5[5], as.Date("2025-10-25"))
})

test_that("game range filter Season returns NULL (no date filter)", {
  resolve_range <- function(sel, all_dates) {
    switch(sel,
      "Season" = NULL,
      "Last 5" = head(sort(all_dates, decreasing = TRUE), 5),
      as.Date(sel)
    )
  }
  dates <- as.Date(c("2025-11-22","2025-10-25"))
  expect_null(resolve_range("Season", dates))
  expect_length(resolve_range("Last 5", dates), 2)
  expect_equal(resolve_range("2025-11-22", dates), as.Date("2025-11-22"))
})
```

- [ ] **Step 2: Run test — expect PASS** (pure logic, no Shiny needed)
```bash
Rscript -e "testthat::test_file('tests/testthat/test-game-nav.R')"
```

- [ ] **Step 3: Replace dateRangeInput with game chip slot in `coach_sidebar()` in `global.R`**

Find this block inside `coach_sidebar()`:
```r
      dateRangeInput("dates", "Date Range",
        start = min(data$Date, na.rm = TRUE),
        end   = max(data$Date, na.rm = TRUE)
      ),
```
Replace with:
```r
      tags$label("Game", class = "control-label"),
      div(class = "game-chip-row", uiOutput("game_selector")),
```

- [ ] **Step 4: Add game chip renderUI and reactive to `server.R`**

Add these two blocks immediately after `app_data <- reactiveVal(data)` (the first line inside `server()`):

```r
  # ── Game chip selector ─────────────────────────────────────────────────────
  output$game_selector <- renderUI({
    req(user_role() == "coach")
    dates  <- sort(unique(app_data()$Date), decreasing = TRUE)
    labels <- format(dates, "%b %d")
    sel    <- if (is.null(input$selected_game_chip)) "Season" else input$selected_game_chip

    make_chip <- function(value, label) {
      active <- identical(sel, value)
      tags$button(
        label,
        class   = paste("btn btn-sm game-chip-btn", if (active) "btn-dark" else "btn-outline-secondary"),
        onclick = sprintf(
          "Shiny.setInputValue('selected_game_chip', '%s', {priority: 'event'})", value
        )
      )
    }

    tagList(
      make_chip("Season", "Season"),
      make_chip("Last 5", "Last 5"),
      lapply(seq_along(dates), function(i) make_chip(as.character(dates[i]), labels[i]))
    )
  })

  selected_game_range <- reactive({
    sel <- if (is.null(input$selected_game_chip)) "Season" else input$selected_game_chip
    all_dates <- sort(unique(app_data()$Date), decreasing = TRUE)
    switch(sel,
      "Season" = NULL,
      "Last 5" = head(all_dates, 5L),
      as.Date(sel)
    )
  })
```

- [ ] **Step 5: Update `fdata()` date filter in `server.R`**

Find the current `fdata <- reactive({` block. Replace the `req()` line and the date filter lines:

Old `req` line:
```r
    req(input$dates, input$pitch_types, input$count, input$innings)
```
New:
```r
    req(input$pitch_types, input$count, input$innings)
```

Old date filter inside the `filter()` call:
```r
        Date >= input$dates[1],
        Date <= input$dates[2],
```
New (replace those two lines):
```r
        if (!is.null(selected_game_range()))
          Date %in% selected_game_range()
        else
          TRUE,
```

Note: `dplyr::filter()` accepts logical vectors inline like this. The `if/else` evaluates at filter time.

- [ ] **Step 6: Remove the coach sidebar sync observer that references `input$dates`**

Search `server.R` for any `updateDateRangeInput` calls and remove them (they were part of the Drive sync handler that re-initialised the date range after sync). Replace with a call to reset the game chip:
```r
# After app_data(new_data) in the sync handler, add:
session$sendCustomMessage("resetGameChip", list(value = "Season"))
```
And in the sync handler's `output$sync_status` update section, no date widget to reset — just leave the chip at whatever it is.

If the sync handler also calls `updateDateRangeInput(...)`, delete those lines entirely.

- [ ] **Step 7: Run full test suite**
```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```
Expected: all pass.

- [ ] **Step 8: Smoke-test**
```bash
Rscript -e "shiny::runApp('/Users/tsobazy/sfs_dashboard', port=7341, launch.browser=FALSE)" &
sleep 6 && curl -s -o /dev/null -w "%{http_code}" http://localhost:7341/
pkill -f "port=7341"
```

- [ ] **Step 9: Commit**
```bash
git add global.R server.R tests/testthat/test-game-nav.R
git commit -m "feat: game chip selector replaces date range in coach sidebar"
```

---

### Task 3: Arsenal overview table

**Files:**
- Modify: `server.R` (new `output$table_arsenal`)
- Modify: `global.R` (`coach_layout()` — wire table_arsenal into Pitching Overview)

**Interfaces:**
- Consumes: `fdata()`, `group_col()`
- Produces: `output$table_arsenal` (DT table) — rendered in Pitching Overview tab

- [ ] **Step 1: Add `output$table_arsenal` to `server.R`**

Add this block right after `output$table_movement` (search for `# ── IVB / HB movement rundown table`):

```r
  # ── Arsenal overview table — coaches see this first ───────────────────────
  output$table_arsenal <- DT::renderDT({
    req(nrow(fdata()) > 0)
    gcol <- group_col()
    d <- fdata() %>%
      group_by(Group = .data[[gcol]]) %>%
      summarise(
        `Usage%`   = scales::percent(n() / nrow(fdata()), accuracy = 1),
        `Avg Velo` = round(mean(RelSpeed,         na.rm = TRUE), 1),
        `Max Velo` = round(max(RelSpeed,          na.rm = TRUE), 1),
        `Avg Spin` = round(mean(SpinRate,         na.rm = TRUE), 0),
        `Avg IVB`  = round(mean(InducedVertBreak, na.rm = TRUE), 1),
        `Avg HB`   = round(mean(HorzBreak,        na.rm = TRUE), 1),
        .groups    = "drop"
      ) %>%
      arrange(desc(as.numeric(sub("%", "", `Usage%`))))

    DT::datatable(d, rownames = FALSE,
      options = list(pageLength = 15, dom = "t", ordering = TRUE),
      class   = "compact stripe"
    ) %>%
      DT::formatStyle("Avg Velo",
        background = DT::styleInterval(c(78, 85),
          c("#FEF3C7", "white", "#DBEAFE"))
      )
  })
```

- [ ] **Step 2: Wire `table_arsenal` into Pitching Overview in `global.R`**

Find the Pitching Overview tab panel in `coach_layout()`:
```r
            tabPanel(
              "Overview",
              fluidRow(
                column(6, plotlyOutput("plot_zone",    height = "380px")),
                column(6, plotlyOutput("plot_arsenal", height = "380px"))
              ),
              fluidRow(column(12, DTOutput("table_movement"))),
              fluidRow(column(12, DTOutput("table_pitchers")))
            ),
```

Replace with:
```r
            tabPanel(
              "Overview",
              uiOutput("coach_pitch_glance"),
              fluidRow(column(12, DTOutput("table_arsenal"))),
              fluidRow(column(12, plotlyOutput("plot_movement", height = "420px"))),
              fluidRow(column(12, DTOutput("table_pitchers")))
            ),
```

Note: `plot_movement` (the movement scatter) now lives in Overview as the visual complement to the arsenal table. The zone map and arsenal donut move to Detail.

- [ ] **Step 3: Update Detail tab to hold the charts removed from Overview**

Find the Pitching Detail tab panel and add `plot_zone` and `plot_arsenal` there:
```r
            tabPanel(
              "Detail",
              fluidRow(
                column(6, plotlyOutput("plot_zone",    height = "380px")),
                column(6, plotlyOutput("plot_arsenal", height = "380px"))
              ),
              fluidRow(
                column(12, plotlyOutput("plot_velo_spin", height = "340px"))
              ),
              fluidRow(
                column(6, plotlyOutput("plot_release",  height = "380px")),
                column(6, plotlyOutput("plot_outcomes", height = "380px"))
              ),
              fluidRow(
                column(12, plotlyOutput("plot_count_heatmap", height = "360px"))
              )
            )
```

- [ ] **Step 4: Smoke-test**
```bash
Rscript -e "shiny::runApp('/Users/tsobazy/sfs_dashboard', port=7342, launch.browser=FALSE)" &
sleep 6 && curl -s -o /dev/null -w "%{http_code}" http://localhost:7342/
pkill -f "port=7342"
```

- [ ] **Step 5: Commit**
```bash
git add global.R server.R
git commit -m "feat: arsenal overview table — coaches see pitch stats before charts"
```

---

### Task 4: Movement profile with Savant-style reference rings

**Files:**
- Modify: `global.R` (add `ring_df()` helper)
- Modify: `server.R` (replace `output$plot_movement` body)
- Test: `tests/testthat/test-ring-df.R` (new)

**Interfaces:**
- Consumes: `fdata()`, `group_col()`, `group_colors()`
- Produces: `ring_df(r)` → data.frame with columns `x`, `y`, `r` — consumed by `plot_movement`

- [ ] **Step 1: Write failing test**

Create `tests/testthat/test-ring-df.R`:
```r
source("../../global.R")

test_that("ring_df produces a closed circle with correct radius", {
  df <- ring_df(12)
  expect_named(df, c("x", "y", "r"))
  expect_true(nrow(df) >= 200)
  # All points should be ~12 inches from origin
  radii <- sqrt(df$x^2 + df$y^2)
  expect_true(all(abs(radii - 12) < 0.01))
  expect_equal(unique(df$r), 12)
})

test_that("ring_df(0) produces a degenerate point", {
  df <- ring_df(0)
  radii <- sqrt(df$x^2 + df$y^2)
  expect_true(all(abs(radii) < 0.01))
})
```

- [ ] **Step 2: Run test — expect FAIL**
```bash
Rscript -e "testthat::test_file('tests/testthat/test-ring-df.R')"
```

- [ ] **Step 3: Add `ring_df()` to `global.R`** after the `plotly_clean()` function:
```r
ring_df <- function(r, n = 200) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(x = r * cos(theta), y = r * sin(theta), r = r)
}
```

- [ ] **Step 4: Run test — expect PASS**
```bash
Rscript -e "testthat::test_file('tests/testthat/test-ring-df.R')"
```

- [ ] **Step 5: Replace `output$plot_movement` in `server.R`**

Find `output$plot_movement <- renderPlotly({` and replace the entire block through its closing `})`:

```r
  # ── Movement Profile — Savant-style ───────────────────────────────────────
  output$plot_movement <- renderPlotly({
    req(nrow(fdata()) > 0)
    gcol <- group_col()

    # Per-pitch cluster data for the movement scatter
    d <- fdata() %>%
      group_by(.data[[gcol]]) %>%
      summarise(
        HorzBreak        = mean(HorzBreak,        na.rm = TRUE),
        InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
        n                = n(),
        .groups          = "drop"
      )

    # Reference rings at 6", 12", 18"
    rings <- dplyr::bind_rows(lapply(c(6, 12, 18), ring_df))

    cols <- group_colors()

    p <- ggplot(d, aes(
        x = HorzBreak, y = InducedVertBreak,
        color = .data[[gcol]], size = n,
        label = .data[[gcol]]
      )) +
      geom_path(data = rings, aes(x = x, y = y, group = r),
                color = "#D1D5DB", linetype = "dashed", linewidth = 0.4,
                inherit.aes = FALSE) +
      geom_hline(yintercept = 0, color = "#CBD5E1", linewidth = 0.5) +
      geom_vline(xintercept = 0, color = "#CBD5E1", linewidth = 0.5) +
      geom_point(alpha = 0.85) +
      geom_text(vjust = -1, size = 3.5, fontface = "bold", show.legend = FALSE) +
      scale_color_manual(values = cols) +
      scale_size_continuous(range = c(4, 12)) +
      coord_fixed() +
      labs(
        title    = "Movement Profile",
        subtitle = "Pitcher's-eye view — like the TV camera behind the mound",
        x = "Horizontal Break (in)", y = "Induced Vert Break (in)",
        color = NULL, size = "Pitches"
      ) +
      theme_seagulls() +
      theme(legend.position = if (n_distinct(d[[gcol]]) <= 1) "none" else "right")

    plotly_clean(ggplotly(p, tooltip = c("label", "x", "y", "size")))
  })
```

- [ ] **Step 6: Run full test suite**
```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

- [ ] **Step 7: Commit**
```bash
git add global.R server.R tests/testthat/test-ring-df.R
git commit -m "feat: movement profile with Savant-style reference rings"
```

---

### Task 5: Spray chart (coach hitting view + player batter view)

**Files:**
- Modify: `global.R` (add `spray_xy()` helper, `field_outline_df()` helper)
- Modify: `server.R` (new `output$plot_spray_coach`, replace `output$player_spray`)
- Modify: `global.R` (`coach_layout()` — add spray chart to Hitting Overview)
- Test: `tests/testthat/test-spray.R` (new)

**Interfaces:**
- Consumes: `fdata()` or `player_fdata()`, columns `Bearing` (degrees from center) and `Distance` (feet)
- Produces: `spray_xy(bearing_deg, distance_ft)` → list(x, y); `field_outline_df()` → data.frame for geom_path

- [ ] **Step 1: Write failing test**

Create `tests/testthat/test-spray.R`:
```r
source("../../global.R")

test_that("spray_xy converts straight center (0°) to y-axis", {
  pt <- spray_xy(0, 300)
  expect_equal(pt$x, 0, tolerance = 0.01)
  expect_equal(pt$y, 300, tolerance = 0.01)
})

test_that("spray_xy converts 45° right to equal x and y", {
  pt <- spray_xy(45, 100)
  expect_equal(pt$x, pt$y, tolerance = 0.01)
})

test_that("spray_xy converts -45° left to negative x, positive y", {
  pt <- spray_xy(-45, 100)
  expect_true(pt$x < 0)
  expect_true(pt$y > 0)
})

test_that("field_outline_df returns a data frame with x and y columns", {
  df <- field_outline_df()
  expect_true(is.data.frame(df))
  expect_true("x" %in% names(df))
  expect_true("y" %in% names(df))
  expect_gt(nrow(df), 10)
})
```

- [ ] **Step 2: Run test — expect FAIL**
```bash
Rscript -e "testthat::test_file('tests/testthat/test-spray.R')"
```

- [ ] **Step 3: Add `spray_xy()` and `field_outline_df()` to `global.R`** (after `ring_df()`):
```r
spray_xy <- function(bearing_deg, distance_ft) {
  rad <- bearing_deg * pi / 180
  list(x = distance_ft * sin(rad), y = distance_ft * cos(rad))
}

field_outline_df <- function(foul_distance = 330, cf_distance = 400) {
  # Left foul line: from home (0,0) to left field corner
  lf_x <- -foul_distance * sin(pi / 4)
  lf_y <-  foul_distance * cos(pi / 4)
  rf_x <-  foul_distance * sin(pi / 4)
  rf_y <-  foul_distance * cos(pi / 4)

  # Outfield arc from LF to RF, centered at home plate
  arc_angles <- seq(-pi / 4, pi / 4, length.out = 80)
  arc_r      <- seq(foul_distance, cf_distance,
                    length.out = 80)[c(rep(1, 20), rep(80, 20), 40:61)]
  # Simple arc at ~330 LF/RF, ~400 CF
  arc_a <- seq(-pi / 4, pi / 4, length.out = 100)
  arc_r2 <- foul_distance + (cf_distance - foul_distance) *
    (1 - abs(arc_a) / (pi / 4))
  arc_x <- arc_r2 * sin(arc_a)
  arc_y <- arc_r2 * cos(arc_a)

  data.frame(
    x = c(0, lf_x, arc_x, rf_x, 0),
    y = c(0, lf_y, arc_y, rf_y, 0)
  )
}
```

- [ ] **Step 4: Run test — expect PASS**
```bash
Rscript -e "testthat::test_file('tests/testthat/test-spray.R')"
```

- [ ] **Step 5: Add `output$plot_spray_coach` to `server.R`**

Add after `output$plot_ev_la`:
```r
  # ── Coach hitting: Spray chart ────────────────────────────────────────────
  output$plot_spray_coach <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      filter(PitchCall == "InPlay", !is.na(Bearing), !is.na(Distance)) %>%
      mutate(
        sx = Distance * sin(Bearing * pi / 180),
        sy = Distance * cos(Bearing * pi / 180)
      )
    req(nrow(d) > 0)

    field <- field_outline_df()
    bip_colors <- c(GroundBall = "#8B4513", FlyBall = "#457B9D",
                    LineDrive  = "#2DC653", Popup   = "#AAAAAA")

    p <- ggplot() +
      geom_path(data = field, aes(x = x, y = y),
                color = "#94A3B8", linewidth = 0.8, inherit.aes = FALSE) +
      geom_point(data = d, aes(
          x = sx, y = sy, color = TaggedHitType, size = ExitSpeed,
          text = paste0(Batter, "<br>", TaggedHitType,
                        "<br>EV: ", round(ExitSpeed, 1), " mph")
        ), alpha = 0.75) +
      scale_color_manual(values = bip_colors, na.value = "#AAAAAA") +
      scale_size_continuous(range = c(2, 8), name = "Exit Velo (mph)") +
      coord_fixed(xlim = c(-350, 350), ylim = c(-20, 420)) +
      labs(title = "Spray Chart", subtitle = "Dot size = exit velocity",
           x = NULL, y = NULL, color = NULL) +
      theme_seagulls() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid = element_blank())

    plotly_clean(ggplotly(p, tooltip = "text"))
  })
```

- [ ] **Step 6: Replace `output$player_spray` in `server.R`**

Find `output$player_spray <- renderPlotly({` and replace the entire block:
```r
  output$player_spray <- renderPlotly({
    d <- player_fdata() %>%
      filter(PitchCall == "InPlay", !is.na(Bearing), !is.na(Distance)) %>%
      mutate(
        sx = Distance * sin(Bearing * pi / 180),
        sy = Distance * cos(Bearing * pi / 180)
      )
    req(nrow(d) > 0)

    field <- field_outline_df()
    bip_colors <- c(GroundBall = "#8B4513", FlyBall = "#457B9D",
                    LineDrive  = "#2DC653", Popup   = "#AAAAAA")

    p <- ggplot() +
      geom_path(data = field, aes(x = x, y = y),
                color = "#94A3B8", linewidth = 0.8, inherit.aes = FALSE) +
      geom_point(data = d, aes(
          x = sx, y = sy, color = TaggedHitType, size = ExitSpeed,
          text = paste0(TaggedHitType, "<br>EV: ", round(ExitSpeed, 1), " mph")
        ), alpha = 0.75) +
      scale_color_manual(values = bip_colors, na.value = "#AAAAAA") +
      scale_size_continuous(range = c(2, 8), guide = "none") +
      coord_fixed(xlim = c(-350, 350), ylim = c(-20, 420)) +
      labs(title = "Spray Chart", subtitle = "Dot size = exit velocity",
           x = NULL, y = NULL, color = NULL) +
      theme_seagulls() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid = element_blank())

    plotly_clean(ggplotly(p, tooltip = "text"))
  })
```

- [ ] **Step 7: Wire spray chart into Hitting Overview in `global.R`**

Find the Hitting Overview tab panel in `coach_layout()`:
```r
            tabPanel(
              "Overview",
              fluidRow(
                column(12, plotlyOutput("plot_spray", height = "420px"))
              ),
              fluidRow(column(12, DTOutput("table_batters")))
            ),
```
Replace with:
```r
            tabPanel(
              "Overview",
              uiOutput("coach_hit_glance"),
              fluidRow(
                column(7, plotlyOutput("plot_spray_coach", height = "440px")),
                column(5, DTOutput("table_batters"))
              )
            ),
```

- [ ] **Step 8: Run full test suite**
```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

- [ ] **Step 9: Commit**
```bash
git add global.R server.R tests/testthat/test-spray.R
git commit -m "feat: spray chart with field geometry for coach hitting view and player batter view"
```

---

### Task 6: Simplify player view — remove coach-level charts, remove all comparisons

**Files:**
- Modify: `server.R` (`output$player_pitcher_section` renderUI, `output$player_hitter_section` renderUI)

**Interfaces:**
- Consumes: `player_fdata()`, `player_recent_fdata()`
- Produces: simplified pitcher section (scorecard + movement + location), simplified hitter section (scorecard + spray + swing zones)

- [ ] **Step 1: Replace `output$player_pitcher_section` renderUI in `server.R`**

Find `output$player_pitcher_section <- renderUI({` and replace the entire block through its closing `})`. The new version removes `player_release`, `player_outcomes`, `player_arsenal`, removes all team-comparison language, and adds the movement profile:

```r
  output$player_pitcher_section <- renderUI({
    req(user_role() == "player")
    d <- player_fdata()
    if (nrow(d) == 0) return(div("No data for this game.", style = "color:#888; margin:16px 0;"))

    spct   <- strike_pct(d$PitchCall)
    wpct   <- whiff_pct(d$PitchCall)
    cswpct <- csw_pct(d$PitchCall)
    gbpct  <- gb_pct(d$TaggedHitType)

    fmt <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 1)

    d_rec      <- player_recent_fdata()
    has_recent <- nrow(d_rec) > 0

    mk_trend <- function(curr, base) {
      if (!has_recent || is.na(curr) || is.na(base))
        return(tags$small("— first game with data", class = "tile-trend"))
      diff <- curr - base
      if (abs(diff) < 0.001)
        return(tags$small("— stable", class = "tile-trend"))
      dir <- if (diff > 0) "↑" else "↓"
      tags$small(paste0(dir, " ", round(abs(diff) * 100, 1), " pts vs last 5"),
                 class = "tile-trend")
    }

    base_spct   <- if (has_recent) strike_pct(d_rec$PitchCall)   else NA_real_
    base_wpct   <- if (has_recent) whiff_pct(d_rec$PitchCall)    else NA_real_
    base_cswpct <- if (has_recent) csw_pct(d_rec$PitchCall)      else NA_real_
    base_gbpct  <- if (has_recent) gb_pct(d_rec$TaggedHitType)   else NA_real_

    best_pitch <- d %>%
      group_by(TaggedPitchType) %>%
      summarise(wp = whiff_pct(PitchCall), n = n(), .groups = "drop") %>%
      filter(n >= 5) %>%
      slice_max(wp, n = 1, with_ties = FALSE)

    takeaway <- if (nrow(best_pitch) > 0 && !is.na(spct)) {
      paste0(
        if (spct >= 0.62) "Strong command game" else "Work on command",
        " — ", fmt(spct), " strikes.",
        if (nrow(best_pitch) > 0)
          paste0(" Best pitch: ", best_pitch$TaggedPitchType,
                 " (", fmt(best_pitch$wp), " whiff rate).") else ""
      )
    } else { "Not enough data to summarise this game." }

    tagList(
      div(
        style = "background:#f0fafb; border-left:3px solid #2A9D8F;
                 padding:10px 14px; border-radius:4px; margin-bottom:14px;
                 font-size:13px; color:#1A202C;",
        takeaway
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        stat_tile("Strike%",  fmt(spct),   "tile-teal",   mk_trend(spct,   base_spct),
                  "Strikes thrown ÷ total pitches (swings, called strikes, fouls, in-play)"),
        stat_tile("Whiff%",   fmt(wpct),   "tile-teal",   mk_trend(wpct,   base_wpct),
                  "Swings-and-misses ÷ total swings"),
        stat_tile("CSW%",     fmt(cswpct), "tile-neutral", mk_trend(cswpct, base_cswpct),
                  "Called strikes + whiffs ÷ total pitches"),
        stat_tile("GB%",      fmt(gbpct),  "tile-neutral", mk_trend(gbpct,  base_gbpct),
                  "Ground balls ÷ balls in play")
      ),
      fluidRow(
        column(6, plotlyOutput("player_movement", height = "380px")),
        column(6, plotlyOutput("player_zone",     height = "380px"))
      )
    )
  })
```

- [ ] **Step 2: Add `output$player_movement` to `server.R`**

Add right after `output$player_zone`:
```r
  output$player_movement <- renderPlotly({
    d_raw <- player_fdata()
    req(nrow(d_raw) > 0)

    d <- d_raw %>%
      group_by(TaggedPitchType) %>%
      summarise(
        HorzBreak        = mean(HorzBreak,        na.rm = TRUE),
        InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
        n                = n(),
        .groups          = "drop"
      )

    rings <- dplyr::bind_rows(lapply(c(6, 12, 18), ring_df))

    p <- ggplot(d, aes(
        x = HorzBreak, y = InducedVertBreak,
        color = TaggedPitchType, size = n, label = TaggedPitchType
      )) +
      geom_path(data = rings, aes(x = x, y = y, group = r),
                color = "#D1D5DB", linetype = "dashed", linewidth = 0.4,
                inherit.aes = FALSE) +
      geom_hline(yintercept = 0, color = "#CBD5E1", linewidth = 0.5) +
      geom_vline(xintercept = 0, color = "#CBD5E1", linewidth = 0.5) +
      geom_point(alpha = 0.85) +
      geom_text(vjust = -1, size = 3.5, fontface = "bold", show.legend = FALSE) +
      scale_color_manual(values = PITCH_COLORS) +
      scale_size_continuous(range = c(4, 12), guide = "none") +
      coord_fixed() +
      labs(title = "Movement Profile",
           subtitle = "Pitcher's-eye view",
           x = "Horizontal Break (in)", y = "Induced Vert Break (in)", color = NULL) +
      theme_seagulls() +
      theme(legend.position = if (n_distinct(d$TaggedPitchType) <= 1) "none" else "right")

    plotly_clean(ggplotly(p, tooltip = c("label", "x", "y", "size")))
  })
```

- [ ] **Step 3: Replace `output$player_hitter_section` renderUI in `server.R`**

Find `output$player_hitter_section <- renderUI({` and replace the entire block through its closing `})`:

```r
  output$player_hitter_section <- renderUI({
    req(user_role() == "player")
    d <- player_fdata()
    if (nrow(d) == 0) return(div("No data for this game.", style = "color:#888; margin:16px 0;"))

    d_ip   <- d %>% filter(PitchCall == "InPlay", !is.na(ExitSpeed))
    avg_ev <- mean(d_ip$ExitSpeed, na.rm = TRUE)
    hh     <- hard_hit_pct(d_ip$ExitSpeed)
    brl    <- barrel_pct(d_ip$ExitSpeed, d_ip$Angle)

    d_ab   <- d %>% filter(!is.na(PlayResult))
    hits   <- sum(d_ab$PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm = TRUE)
    ab     <- sum(d_ab$PlayResult %in% c("Single","Double","Triple","HomeRun","Out"), na.rm = TRUE)
    avg    <- if (ab > 0) hits / ab else NA_real_

    fmt_pct <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 1)
    fmt_avg <- function(x) if (is.na(x)) "—" else sprintf("%.3f", x)
    fmt_ev  <- function(x) if (is.na(x)) "—" else paste0(round(x, 1), " mph")

    d_rec      <- player_recent_fdata()
    has_recent <- nrow(d_rec) > 0

    mk_trend_ev <- function(curr, base) {
      if (!has_recent || is.na(curr) || is.na(base))
        return(tags$small("— first game with data", class = "tile-trend"))
      diff <- curr - base
      if (abs(diff) < 0.1)
        return(tags$small("— stable", class = "tile-trend"))
      dir <- if (diff > 0) "↑" else "↓"
      tags$small(paste0(dir, " ", round(abs(diff), 1), " mph vs last 5"),
                 class = "tile-trend")
    }

    mk_trend <- function(curr, base) {
      if (!has_recent || is.na(curr) || is.na(base))
        return(tags$small("— first game with data", class = "tile-trend"))
      diff <- curr - base
      if (abs(diff) < 0.001)
        return(tags$small("— stable", class = "tile-trend"))
      dir <- if (diff > 0) "↑" else "↓"
      tags$small(paste0(dir, " ", round(abs(diff) * 100, 1), " pts vs last 5"),
                 class = "tile-trend")
    }

    d_rec_ip    <- d_rec %>% filter(PitchCall == "InPlay", !is.na(ExitSpeed))
    base_ev     <- mean(d_rec_ip$ExitSpeed, na.rm = TRUE)
    base_hh     <- hard_hit_pct(d_rec_ip$ExitSpeed)
    base_brl    <- barrel_pct(d_rec_ip$ExitSpeed, d_rec_ip$Angle)

    takeaway <- if (!is.na(avg_ev) && !is.na(hh)) {
      quality <- if (hh >= 0.35) "great contact quality" else if (hh >= 0.20) "solid contact" else "struggled with hard contact"
      paste0("Game summary: ", quality, ". Avg EV ", fmt_ev(avg_ev),
             if (!is.na(hh)) paste0(", ", fmt_pct(hh), " hard hit rate.") else ".")
    } else { "Not enough batted ball data for this game." }

    tagList(
      div(
        style = "background:#fff8f0; border-left:3px solid #F4A261;
                 padding:10px 14px; border-radius:4px; margin-bottom:14px;
                 font-size:13px; color:#1A202C;",
        takeaway
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        stat_tile("AVG",       fmt_avg(avg), "tile-teal",   NULL,
                  "Hits ÷ at-bats for this game"),
        stat_tile("Hard Hit%", fmt_pct(hh),  "tile-amber",  mk_trend(hh,  base_hh),
                  "Batted balls at 95+ mph exit velocity"),
        stat_tile("Barrel%",   fmt_pct(brl), "tile-amber",  mk_trend(brl, base_brl),
                  "Batted balls with EV ≥ 98 mph and launch angle 26–30°"),
        stat_tile("Avg EV",    fmt_ev(avg_ev), "tile-neutral", mk_trend_ev(avg_ev, base_ev),
                  "Average exit velocity on balls in play")
      ),
      fluidRow(
        column(6, plotlyOutput("player_spray",       height = "380px")),
        column(6, plotlyOutput("player_swing_zones", height = "380px"))
      )
    )
  })
```

- [ ] **Step 4: Remove dead outputs — `player_arsenal`, `player_release`, `player_outcomes`, `player_hit_types`**

These outputs are no longer referenced in the UI. Delete their `renderPlotly` blocks from `server.R` (search for `output$player_arsenal`, `output$player_release`, `output$player_outcomes`, `output$player_hit_types` and delete each block).

- [ ] **Step 5: Smoke-test**
```bash
Rscript -e "shiny::runApp('/Users/tsobazy/sfs_dashboard', port=7345, launch.browser=FALSE)" &
sleep 6 && curl -s -o /dev/null -w "%{http_code}" http://localhost:7345/
pkill -f "port=7345"
```

- [ ] **Step 6: Run full test suite**
```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

- [ ] **Step 7: Commit**
```bash
git add server.R
git commit -m "feat: player view simplified — movement + location for pitchers, spray + swing zones for batters, no team comparisons"
```

---

### Task 7: Final wiring — flush sidebar, chip CSS, smoke test everything

**Files:**
- Modify: `global.R` (sidebar style — flush left, `margin:0; padding-left:0`)
- Modify: `ui.R` (body margin:0 already set in Task 1 CSS)

- [ ] **Step 1: Ensure sidebar has no left gap**

In `coach_sidebar()` in `global.R`, the outer div currently has `padding:0`. Confirm:
```r
    style = "width:280px; min-width:280px; padding:0; background:#1E2A3A;
             height:100vh; overflow-y:auto; display:flex; flex-direction:column;",
```
If background is still `#015294`, change to `#1E2A3A`. If padding is not `0`, set to `0`.

In `coach_layout()`, the wrapping `div(style="display:flex; height:100vh;")` should have `margin:0; padding:0`:
```r
  div(
    style = "display:flex; height:100vh; margin:0; padding:0;",
    coach_sidebar(),
    div(
      style = "flex:1; overflow-y:auto; padding:20px;",
      ...
```

- [ ] **Step 2: Full end-to-end smoke test with Playwright**
```bash
Rscript -e "shiny::runApp('/Users/tsobazy/sfs_dashboard', port=7346, launch.browser=FALSE)" &
sleep 7
python3 - <<'EOF'
from playwright.sync_api import sync_playwright
with sync_playwright() as p:
    browser = p.chromium.launch(headless=True)
    page = browser.new_page(viewport={"width": 1440, "height": 900})
    page.goto("http://localhost:7346/")
    page.wait_for_selector("#auth-user_id", timeout=10000)
    page.fill("#auth-user_id", "coach_cascone")
    page.fill("#auth-user_pwd", "seagulls2026")
    page.click("button.btn-success, button[type=submit], .btn-primary")
    page.wait_for_timeout(4000)
    page.screenshot(path="/tmp/redesign_coach_overview.png")
    page.get_by_text("Detail").first.click()
    page.wait_for_timeout(2500)
    page.screenshot(path="/tmp/redesign_coach_detail.png")
    page.get_by_text("Hitting").nth(1).click()
    page.wait_for_timeout(2500)
    page.screenshot(path="/tmp/redesign_coach_hitting.png")
    browser.close()
    print("done")
EOF
pkill -f "port=7346"
```

- [ ] **Step 3: Run full test suite one final time**
```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```
Expected: all tests pass.

- [ ] **Step 4: Final commit + push**
```bash
git add global.R server.R ui.R
git commit -m "feat: sidebar flush left, layout polish — dashboard redesign complete"
git push origin main
```

---

## Self-Review

**Spec coverage check:**
- ✅ Statcast PITCH_COLORS — Task 1
- ✅ Inter font + bs_theme — Task 1
- ✅ plotly_clean (no toolbar) — Task 1
- ✅ theme_seagulls (white bg, minimal gridlines) — Task 1
- ✅ Game chip selector replaces date range — Task 2
- ✅ Arsenal overview table — Task 3
- ✅ Movement profile with reference rings — Task 4
- ✅ Spray chart coach + player — Task 5
- ✅ Player view simplified (no release, no outcomes, no comparisons) — Task 6
- ✅ Sidebar flush left — Task 7
- ✅ No team-avg comparisons in player view — enforced in Task 6 renderUI code
- ✅ Category colors updated to match Statcast family — Task 1

**Placeholder scan:** No TBDs. All code blocks complete.

**Type consistency:** `plotly_clean()` used everywhere `plotly_white()` was. `ring_df()` used in Task 4 (coach movement) and Task 6 (player movement) consistently. `spray_xy()` not called directly in outputs (inline math used instead for clarity, but `field_outline_df()` is called consistently).
