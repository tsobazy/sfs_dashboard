library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(shinymanager)
library(scales)

# Customise shinymanager login screen labels
shinymanager::set_labels(
  "en",
  "Please authenticate" = "Please login",
  "Username:"           = "Username",
  "Password:"           = "Password",
  "Login"               = "Sign In"
)

# ── Auto-sync game CSVs from Google Drive on startup ────────────────────────────
# Pulls any new CSVs from the configured Drive folder and rebuilds
# all_fall_25.csv before the data is read below. Runs only when a folder ID is
# set and Drive auth (.secrets/) exists, and never blocks startup: any failure
# (offline, expired token, empty folder) is caught so the app still boots on the
# CSVs already present locally.
source("sync_drive.R")
local({
  folder_id <- Sys.getenv("SEAGULLS_DRIVE_FOLDER_ID")
  if (nchar(folder_id) == 0 || !dir.exists(".secrets")) return(invisible())
  tryCatch({
    n_new <- sync_from_drive(folder_id)
    build_combined_csv()
    message("Drive sync: ", n_new, " new file(s) downloaded on startup.")
  }, error = function(e) {
    message("Drive sync skipped on startup: ", conditionMessage(e))
  })
})

# ── Data ──────────────────────────────────────────────────────────────────────
# Real game data only — rebuilt from the Drive CSVs into all_fall_25.csv on
# startup (kept off git as it contains real player data).
if (!file.exists("all_fall_25.csv"))
  stop("all_fall_25.csv not found. Configure Drive sync or drop game CSVs in ",
       "data/game_csvs/ so the app has data to load.")
data <- read_csv("all_fall_25.csv", show_col_types = FALSE)

data$TaggedPitchType <- if_else(
  data$TaggedPitchType %in% c("Other", NA_character_), "Undefined", data$TaggedPitchType
)
data$Count <- paste0(data$Balls, "-", data$Strikes)

# ── Constants ─────────────────────────────────────────────────────────────────
# TrackMan team code for the SF Seagulls. Game CSVs contain both teams, so the
# dashboard filters to this code to show only Seagulls players (our pitchers on
# the Pitching tab, our hitters on the Hitting tab). Matching uses %in%, so add
# more codes here if TrackMan ever tags the Seagulls differently.
SEAGULLS_TEAM <- "SAN_FRA4"

SZ_LEFT  <- -0.83
SZ_RIGHT <-  0.83
SZ_BOT   <-  1.50
SZ_TOP   <-  3.50

PITCH_COLORS <- c(
  FourSeamFastBall = "#D22D49", Fastball        = "#D22D49",
  Sinker           = "#FE9D00", TwoSeamFastBall = "#FE9D00",
  Cutter           = "#933F2C",
  ChangeUp         = "#1DBE3A", Splitter        = "#3BACAC",
  Slider           = "#EEE716", Sweeper         = "#DDB33A",
  Curveball        = "#00D1ED",
  Undefined        = "#AAAAAA"
)

PITCH_CATEGORY_MAP <- c(
  FourSeamFastBall = "Fastball",      Fastball        = "Fastball",
  Sinker           = "Fastball",      TwoSeamFastBall = "Fastball",
  Cutter           = "Fastball",
  Slider           = "Breaking Ball", Curveball       = "Breaking Ball",
  Sweeper          = "Breaking Ball",
  ChangeUp         = "Offspeed",      Splitter        = "Offspeed",
  Undefined        = "Undefined"
)

data$PitchCategory <- PITCH_CATEGORY_MAP[data$TaggedPitchType]
data$PitchCategory[is.na(data$PitchCategory)] <- "Undefined"
data$PitchCategory <- factor(data$PitchCategory,
  levels = c("Fastball", "Breaking Ball", "Offspeed", "Undefined"))
data$Season <- "Summer 2026"

PITCH_CATEGORY_COLORS <- c(
  Fastball        = "#D22D49",
  `Breaking Ball` = "#00D1ED",
  Offspeed        = "#1DBE3A",
  Undefined       = "#AAAAAA"
)

PITCHER_COUNTS <- c("0-1", "0-2", "1-2", "2-2")
HITTER_COUNTS  <- c("1-0", "2-0", "2-1", "3-0", "3-1")
TWO_K_COUNTS   <- c("0-2", "1-2", "2-2", "3-2")

BIP_TYPES <- c("GroundBall", "FlyBall", "LineDrive", "Popup")

# ── Metric Functions ──────────────────────────────────────────────────────────
strike_pct <- function(calls) {
  mean(calls %in% c("StrikeCalled", "StrikeSwinging",
                    "FoulBallNotFieldable", "FoulBallFieldable", "InPlay"),
       na.rm = TRUE)
}

whiff_pct <- function(calls) {
  swings <- calls %in% c("StrikeSwinging", "FoulBallNotFieldable",
                          "FoulBallFieldable", "InPlay")
  n_swings <- sum(swings, na.rm = TRUE)
  if (n_swings == 0) return(NA_real_)
  sum(calls == "StrikeSwinging", na.rm = TRUE) / n_swings
}

csw_pct <- function(calls) {
  mean(calls %in% c("StrikeCalled", "StrikeSwinging"), na.rm = TRUE)
}

chase_pct <- function(side, height, calls) {
  ooz <- side < SZ_LEFT | side > SZ_RIGHT | height < SZ_BOT | height > SZ_TOP
  swings <- calls %in% c("StrikeSwinging", "FoulBallNotFieldable",
                          "FoulBallFieldable", "InPlay")
  n_ooz <- sum(ooz, na.rm = TRUE)
  if (n_ooz == 0) return(NA_real_)
  sum(ooz & swings, na.rm = TRUE) / n_ooz
}

# Hard Hit% — share of balls in play struck at or above HARD_HIT_MPH.
# Set to 85 for this college wood-bat summer league (league avg EV ~79 mph;
# MLB's 95 is unrealistic here and grades nearly everyone at 0%).
HARD_HIT_MPH <- 85

hard_hit_pct <- function(ev) {
  ev <- ev[!is.na(ev)]
  if (length(ev) == 0) return(NA_real_)
  mean(ev >= HARD_HIT_MPH)
}

# Barrel% — share of balls in play with both high exit velo and a productive
# launch angle. MLB uses EV >= 98 with a narrow 26-30 deg band; at this league's
# exit velos that yields ~0 barrels, so we use EV >= 90 (genuinely hard here,
# ~p90 of avg EV) with a wider 20-35 deg productive window (~8% league rate).
BARREL_MPH    <- 90
BARREL_LA_LOW <- 20
BARREL_LA_HI  <- 35

barrel_pct <- function(ev, la) {
  ok <- !is.na(ev) & !is.na(la)
  if (sum(ok) == 0) return(NA_real_)
  mean(ev[ok] >= BARREL_MPH & la[ok] >= BARREL_LA_LOW & la[ok] <= BARREL_LA_HI)
}

gb_pct <- function(ht) {
  bip <- ht[ht %in% BIP_TYPES]
  if (length(bip) == 0) return(NA_real_)
  sum(bip == "GroundBall") / length(bip)
}

# ── Theme ─────────────────────────────────────────────────────────────────────
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

plotly_clean <- function(p) {
  p %>%
    layout(
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      font = list(color = "#1A202C")
    ) %>%
    config(displayModeBar = FALSE)
}

ring_df <- function(r, n = 200) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(x = r * cos(theta), y = r * sin(theta), r = r)
}

spray_xy <- function(bearing_deg, distance_ft) {
  rad <- bearing_deg * pi / 180
  list(x = distance_ft * sin(rad), y = distance_ft * cos(rad))
}

field_outline_df <- function(foul_distance = 330, cf_distance = 400) {
  lf_x <- -foul_distance * sin(pi / 4)
  lf_y <-  foul_distance * cos(pi / 4)
  rf_x <-  foul_distance * sin(pi / 4)
  rf_y <-  foul_distance * cos(pi / 4)
  arc_a  <- seq(-pi / 4, pi / 4, length.out = 100)
  arc_r2 <- foul_distance + (cf_distance - foul_distance) *
    (1 - abs(arc_a) / (pi / 4))
  arc_x <- arc_r2 * sin(arc_a)
  arc_y <- arc_r2 * cos(arc_a)
  data.frame(
    x = c(0, lf_x, arc_x, rf_x, 0),
    y = c(0, lf_y, arc_y, rf_y, 0)
  )
}

# ── 13-Zone Strike Zone ───────────────────────────────────────────────────────
classify_zone13 <- function(side, height) {
  zw    <- (SZ_RIGHT - SZ_LEFT) / 3
  zh    <- (SZ_TOP   - SZ_BOT)  / 3
  mid_y <- (SZ_BOT + SZ_TOP) / 2

  col <- cut(side,   breaks = c(SZ_LEFT, SZ_LEFT + zw, SZ_LEFT + 2*zw, SZ_RIGHT),
             labels = c("L","M","R"), include.lowest = TRUE)
  row <- cut(height, breaks = c(SZ_BOT, SZ_BOT + zh, SZ_BOT + 2*zh, SZ_TOP),
             labels = c("Low","Mid","High"), include.lowest = TRUE)
  inner <- !is.na(col) & !is.na(row)

  in_outer_band <- side   >= SZ_LEFT - zw & side   <= SZ_RIGHT + zw &
                   height >= SZ_BOT  - zh & height <= SZ_TOP   + zh & !inner

  dplyr::case_when(
    inner & col == "L" & row == "High" ~ "1",
    inner & col == "M" & row == "High" ~ "2",
    inner & col == "R" & row == "High" ~ "3",
    inner & col == "L" & row == "Mid"  ~ "4",
    inner & col == "M" & row == "Mid"  ~ "5",
    inner & col == "R" & row == "Mid"  ~ "6",
    inner & col == "L" & row == "Low"  ~ "7",
    inner & col == "M" & row == "Low"  ~ "8",
    inner & col == "R" & row == "Low"  ~ "9",
    in_outer_band & side <  0 & height >= mid_y ~ "11",
    in_outer_band & side >= 0 & height >= mid_y ~ "12",
    in_outer_band & side <  0 & height <  mid_y ~ "13",
    in_outer_band & side >= 0 & height <  mid_y ~ "14",
    TRUE ~ NA_character_
  )
}

# Inner grid: x[-1.5,1.5], y[-1.5,1.5], 1×1 cells centred at origin.
# Outer zones: x/y are label anchor positions near each corner quadrant.
ZONE13_COORDS <- tribble(
  ~zone,  ~x,   ~y,
  "1",   -1,    1,
  "2",    0,    1,
  "3",    1,    1,
  "4",   -1,    0,
  "5",    0,    0,
  "6",    1,    0,
  "7",   -1,   -1,
  "8",    0,   -1,
  "9",    1,   -1,
  "11",  -2,    2,
  "12",   2,    2,
  "13",  -2,   -2,
  "14",   2,   -2
)

source("roster.R")
source("sync_drive.R")

# ── Player photos ─────────────────────────────────────────────────────────────
roster_photos <- tribble(
  ~player_name,          ~photo_url,
  "Bryce Brooks",        "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_bryce_brooks.jpg",
  "Sebastian Ultreras",  "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_sebastian_ultreras.jpg",
  "Declan Mendel",       "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_declan_mendel.jpg",
  "Emilio Feliciano",    "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_emilio_feliciano.jpg",
  "Davis Germann",       "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_davis_germann.jpg",
  "Theodore Tsouras",    "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_theodore_tsouras.jpg",
  "Benjamin Joost",      "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_benjamin_joost.jpg",
  "Finn Whalen",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_finn_whalen.jpg",
  "Matthew Potter",      "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_matthew_potter.jpg",
  "Louden Hilliard",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_louden_hilliard.jpg",
  "Caid Heflin",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_caid_heflin.jpg",
  "Jacob Gilbreath",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_jacob_gilbreath.jpg",
  "Joseph Steidel",      "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_joseph_steidel.jpg",
  "Blake Cowans",        "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_blake_cowans.jpg",
  "Jake Brewer",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_jake_brewer.jpg",
  "Ethan Lopez",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_ethan_lopez.jpg",
  "Caleb Garrison",      "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_caleb_garrison.jpg",
  "Marcus Graham",       "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_marcus_graham.jpg",
  "Connor Wood",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_connor_wood.jpg",
  "Taylor Easthope",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_taylor_easthope.jpg",
  "Derek Waldvogel",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_derek_waldvogel.jpg",
  "Tanner Wall",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_tanner_wall.jpg",
  "Armando Hurtado",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_armando_hurtado.jpg",
  "Brandon Swanson",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_brandon_swanson.jpg",
  "Christian LaMothe",   "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_christian_lamothe.jpg",
  "JB Ferreira",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_jb_ferreira.jpg",
  "Branson Derrington",  "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_branson_derrington.jpg",
  "Camren Boyd",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_camren_boyd.jpg",
  "Luka Shah",           NA_character_,
  "Alan Ramirez",        NA_character_
)

player_photo_tag <- function(player_name, size = "80px") {
  if (is.null(player_name) || is.na(player_name) || nchar(trimws(player_name)) == 0)
    return(tagList())
  slug <- gsub("[^a-z0-9]+", "_", tolower(trimws(player_name)))
  path <- file.path("www/players", paste0(slug, ".jpg"))
  if (file.exists(path)) {
    tags$img(
      src   = paste0("players/", slug, ".jpg"),
      style = paste0("width:", size, "; height:", size,
                     "; border-radius:50%; object-fit:cover; display:block; flex-shrink:0;")
    )
  } else {
    initials <- paste(substr(strsplit(trimws(player_name), "\\s+")[[1]], 1, 1), collapse = "")
    tags$div(
      initials,
      style = paste0(
        "width:", size, "; height:", size, "; border-radius:50%;",
        "background:#2A9D8F; color:white; display:flex; align-items:center;",
        "justify-content:center; font-weight:700; font-size:26px; flex-shrink:0;"
      )
    )
  }
}

# ── Layout helpers (used by server.R renderUI) ─────────────────────────────────

coach_sidebar <- function() {
  div(
    style = "width:280px; min-width:280px; background:#0a1628;
             height:100vh; overflow-y:auto; display:flex; flex-direction:column;",

    # ── Header card ────────────────────────────────────────────────
    div(
      style = "margin:10px; border-radius:12px; overflow:hidden;
               background:#015294; border:1px solid rgba(255,255,255,0.12);
               padding:20px 16px 16px 16px;",

      # Row 1 — Logo, centered
      div(style = "text-align:center; margin-bottom:10px;",
        tags$img(src = "seagulls_logo.png", height = "60px")
      ),

      # Row 2 — Team name, centered
      div(style = "text-align:center; margin-bottom:14px;",
        tags$span("SAN FRANCISCO SEAGULLS",
                  style = "color:#ffffff; font-size:12px; font-weight:700;
                           letter-spacing:1.5px;")
      ),

      # Row 3 — Name/role left, Log Out right
      div(style = "display:flex; align-items:center; justify-content:space-between;",
        div(textOutput("coach_header", inline = TRUE),
            style = "color:#cfe0f0; font-size:13px;"),
        actionButton("logout", "Log Out",
                     style = "font-size:11px; padding:4px 10px; border-radius:20px;
                              background:rgba(255,255,255,0.08);
                              color:#ffffff; border:1px solid rgba(255,255,255,0.3);
                              cursor:pointer;")
      )
    ),

    # ── White filter card ───────────────────────────────────────────
    div(
      style = "background:#ffffff; margin:0 10px 10px 10px; padding:14px;
               border-radius:10px; flex:1; overflow-y:auto;",

      pickerInput(
        "player", "Player",
        choices = c("All Players",
                    sort(unique(data$Pitcher[data$PitcherTeam %in% SEAGULLS_TEAM]))),
        options = list(`live-search` = TRUE)
      ),
      selectInput("season", "Season",
        choices = c("Summer 2026"), selected = "Summer 2026"),
      radioGroupButtons(
        "game_window", label = "Games",
        choices  = c("Last 5", "Last 10", "Full Season", "Custom"),
        selected = "Full Season", justified = TRUE, size = "sm"
      ),
      conditionalPanel(
        condition = "input.game_window == 'Custom'",
        pickerInput("custom_games", label = NULL,
          choices  = NULL, multiple = TRUE,
          options  = list(`actions-box` = TRUE, title = "Select games"))
      ),
      hr(),
      pickerInput(
        "pitch_cats", "Pitch Categories",
        choices  = c("Fastball", "Breaking Ball", "Offspeed"),
        selected = c("Fastball", "Breaking Ball", "Offspeed"),
        multiple = TRUE,
        options  = list(`actions-box` = TRUE,
                        `selected-text-format` = "values",
                        `none-selected-text`   = "None")
      ),
      tags$script(HTML("
        $(document).ready(function() {
          function abbrevPicker() {
            $('.bootstrap-select button .filter-option-inner-inner').each(function() {
              var t = $(this).text();
              t = t.replace(/Fastball/g,'Fast')
                   .replace(/Breaking Ball/g,'Break')
                   .replace(/Offspeed/g,'Off');
              $(this).text(t);
            });
          }
          abbrevPicker();
          $(document).on('changed.bs.select', function() {
            setTimeout(abbrevPicker, 10);
          });
        });
      ")),
      selectInput("count", "Count",
        choices  = c("All", "Pitcher's Count", "Hitter's Count", "2K"),
        selected = "All"),
      sliderInput("innings", "Innings", min = 1, max = 9,
                  value = c(1, 9), step = 1),
      hr(),
      uiOutput("insights")
    )
  )
}

coach_layout <- function() {
  div(
    style = "display:flex; height:100vh; margin:0; padding:0;",
    coach_sidebar(),
    div(
      style = "flex:1; overflow-y:auto; padding:20px;",
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Pitching",
          tabsetPanel(
            id = "pitch_sub_tabs",
            tabPanel(
              "Overview",
              uiOutput("coach_pitch_glance"),
              tags$h5("Pitch Arsenal Summary",
                      style = "font-weight:700; color:#0a1628; font-size:15px; margin:0 0 8px 4px; letter-spacing:0.2px;"),
              fluidRow(column(12, DTOutput("table_arsenal"))),
              fluidRow(
                column(6, plotlyOutput("plot_zone13",  height = "440px")),
                column(6, plotlyOutput("plot_movement", height = "440px"))
              ),
              fluidRow(column(12, DTOutput("table_pitchers")))
            ),
            tabPanel(
              "Detail",
              fluidRow(
                column(1),
                column(3, tags$h5("Fastball",
                  style = "font-weight:700; color:#E63946; text-align:center;
                           font-size:15px; margin:12px 0 4px 0;")),
                column(3, tags$h5("Breaking Ball",
                  style = "font-weight:700; color:#457B9D; text-align:center;
                           font-size:15px; margin:12px 0 4px 0;")),
                column(3, tags$h5("Offspeed",
                  style = "font-weight:700; color:#2A9D8F; text-align:center;
                           font-size:15px; margin:12px 0 4px 0;"))
              ),
              fluidRow(
                column(1, tags$div("Overall",
                  style = "writing-mode:vertical-rl; transform:rotate(180deg);
                           font-weight:600; color:#555; font-size:12px;
                           text-align:center; margin-top:60px;")),
                column(3, plotlyOutput("density_fb_overall",  height = "260px")),
                column(3, plotlyOutput("density_bb_overall",  height = "260px")),
                column(3, plotlyOutput("density_os_overall",  height = "260px"))
              ),
              fluidRow(
                column(1, tags$div("First Pitch",
                  style = "writing-mode:vertical-rl; transform:rotate(180deg);
                           font-weight:600; color:#555; font-size:12px;
                           text-align:center; margin-top:60px;")),
                column(3, plotlyOutput("density_fb_first",    height = "260px")),
                column(3, plotlyOutput("density_bb_first",    height = "260px")),
                column(3, plotlyOutput("density_os_first",    height = "260px"))
              ),
              fluidRow(
                column(1, tags$div("Hitter's Count",
                  style = "writing-mode:vertical-rl; transform:rotate(180deg);
                           font-weight:600; color:#555; font-size:12px;
                           text-align:center; margin-top:60px;")),
                column(3, plotlyOutput("density_fb_hitter",   height = "260px")),
                column(3, plotlyOutput("density_bb_hitter",   height = "260px")),
                column(3, plotlyOutput("density_os_hitter",   height = "260px"))
              ),
              fluidRow(
                column(1, tags$div("Pitcher's Count",
                  style = "writing-mode:vertical-rl; transform:rotate(180deg);
                           font-weight:600; color:#555; font-size:12px;
                           text-align:center; margin-top:60px;")),
                column(3, plotlyOutput("density_fb_pitcher",  height = "260px")),
                column(3, plotlyOutput("density_bb_pitcher",  height = "260px")),
                column(3, plotlyOutput("density_os_pitcher",  height = "260px"))
              ),
              fluidRow(
                column(1, tags$div("2K",
                  style = "writing-mode:vertical-rl; transform:rotate(180deg);
                           font-weight:600; color:#555; font-size:12px;
                           text-align:center; margin-top:60px;")),
                column(3, plotlyOutput("density_fb_2k",       height = "260px")),
                column(3, plotlyOutput("density_bb_2k",       height = "260px")),
                column(3, plotlyOutput("density_os_2k",       height = "260px"))
              )
            ),
            tabPanel(
              "Trends",
              fluidRow(column(12, plotlyOutput("plot_usage_trend", height = "420px")))
            )
          )
        ),
        tabPanel(
          "Hitting",
          tabsetPanel(
            id = "hit_sub_tabs",
            tabPanel(
              "Overview",
              uiOutput("coach_hit_glance"),
              fluidRow(column(12, plotlyOutput("plot_spray", height = "440px"))),
              fluidRow(column(12, DTOutput("table_batters")))
            ),
            tabPanel(
              "Detail",
              fluidRow(
                column(12,
                  tags$h5("Plate Discipline by Situation",
                          style = "font-weight:700; color:#0a1628;
                                   font-size:15px; margin:16px 0 8px 4px;"),
                  DT::DTOutput("table_plate_discipline")
                )
              ),
              fluidRow(column(12,
                div(style = "display:flex; justify-content:center;",
                  plotlyOutput("plot_swing_zones", height = "520px", width = "520px")
                )
              )),
              fluidRow(column(12, plotlyOutput("plot_quality_contact", height = "420px")))
            )
          )
        )
      )
    )
  )
}

player_shell <- function() {
  div(
    style = "display:flex; height:100vh; margin:0; padding:0;",
    # ── Left sidebar — navy, same structure as coach ──────────────────────────
    div(
      style = "width:260px; min-width:260px; padding:0; background:#1E2A3A;
               height:100vh; overflow-y:auto; display:flex; flex-direction:column;",
      # Team branding
      div(
        style = "padding:16px 16px 10px 16px;",
        tags$img(src = "seagulls_logo.png", height = "44px",
                 style = "display:block; margin-bottom:8px;"),
        tags$div("SAN FRANCISCO SEAGULLS",
                 style = "color:#ffffff; font-size:11px; font-weight:700;
                          letter-spacing:1px; margin-bottom:2px;"),
        tags$div("Player Portal",
                 style = "color:#8BA4B8; font-size:11px;")
      ),
      tags$hr(style = "border-color:#2e3f52; margin:8px 0 0;"),
      # Player identity block (photo, name, jersey, position) — server-rendered
      uiOutput("player_sidebar_identity"),
      tags$hr(style = "border-color:#2e3f52; margin:0;"),
      # Game navigator — server-rendered
      uiOutput("player_sidebar_nav"),
      # Push logout to bottom
      div(style = "flex:1;"),
      div(
        style = "padding:16px;",
        actionButton("logout", "Log Out", class = "btn-sm btn-one-light w-100")
      )
    ),
    # ── Main content area ─────────────────────────────────────────────────────
    div(
      style = "flex:1; overflow-y:auto; padding:24px; background:#F8F9FC;",
      uiOutput("player_ui")
    )
  )
}
