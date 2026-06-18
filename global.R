library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(shinymanager)
library(scales)

# ── Data ──────────────────────────────────────────────────────────────────────
data <- read_csv("all_fall_25.csv", show_col_types = FALSE)

data$TaggedPitchType <- if_else(
  data$TaggedPitchType %in% c("Other", NA_character_), "Undefined", data$TaggedPitchType
)
data$Count <- paste0(data$Balls, "-", data$Strikes)

# ── Constants ─────────────────────────────────────────────────────────────────
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
data$Season <- "Fall 2025"

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

hard_hit_pct <- function(ev) {
  ev <- ev[!is.na(ev)]
  if (length(ev) == 0) return(NA_real_)
  mean(ev >= 95)
}

barrel_pct <- function(ev, la) {
  ok <- !is.na(ev) & !is.na(la)
  if (sum(ok) == 0) return(NA_real_)
  mean(ev[ok] >= 98 & la[ok] >= 26 & la[ok] <= 30)
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

ZONE13_COORDS <- tribble(
  ~zone,  ~x,    ~y,
  "1",   -1,    2,
  "2",    0,    2,
  "3",    1,    2,
  "4",   -1,    1,
  "5",    0,    1,
  "6",    1,    1,
  "7",   -1,    0,
  "8",    0,    0,
  "9",    1,    0,
  "11",  -2.1,  1.75,
  "12",   2.1,  1.75,
  "13",  -2.1,  0.25,
  "14",   2.1,  0.25
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
  slug <- tolower(gsub("[^a-z0-9]+", "_", player_name))
  path <- file.path("www/players", paste0(slug, ".jpg"))
  if (file.exists(path)) {
    tags$img(
      src   = paste0("players/", slug, ".jpg"),
      style = paste0("width:", size, "; height:", size,
                     "; border-radius:50%; object-fit:cover;")
    )
  } else {
    tagList()
  }
}

# ── Layout helpers (used by server.R renderUI) ─────────────────────────────────

coach_sidebar <- function() {
  div(
    style = "width:280px; min-width:280px; padding:0; background:#1E2A3A;
             height:100vh; overflow-y:auto; display:flex; flex-direction:column;",

    # Navy header — logo + identity
    div(
      style = "padding:16px 16px 12px 16px;",
      tags$img(src = "seagulls_logo.png", height = "44px",
               style = "display:block; margin-bottom:8px;"),
      tags$div("SAN FRANCISCO SEAGULLS",
               style = "color:#ffffff; font-size:11px; font-weight:700;
                        letter-spacing:1px; margin-bottom:10px;"),
      tags$small(textOutput("coach_header", inline = TRUE),
                 style = "color:#cfe0f0; display:block; margin-bottom:6px;"),
      actionButton("logout", "Log Out", class = "btn-sm btn-one-light")
    ),

    # White card — all filters
    div(
      style = "background:#ffffff; margin:0 12px 12px 12px; padding:14px;
               border-radius:10px; flex:1; overflow-y:auto;",

      radioGroupButtons(
        "view_mode", label = "View",
        choices = c("Pitching", "Hitting"),
        selected = "Pitching", justified = TRUE, size = "sm"
      ),
      hr(),
      pickerInput(
        "player", "Player",
        choices = c("All Players", sort(unique(data$Pitcher))),
        options = list(`live-search` = TRUE)
      ),
      selectInput("season", "Season",
        choices = c("Fall 2025"), selected = "Fall 2025"),
      radioGroupButtons(
        "game_window", "Games",
        choices  = c("Last 5", "Last 10", "Full Season", "Custom"),
        selected = "Last 5", justified = TRUE, size = "sm"
      ),
      conditionalPanel(
        condition = "input.game_window == 'Custom'",
        pickerInput("custom_games", "Select Games",
          choices = NULL, multiple = TRUE,
          options = list(`actions-box` = TRUE))
      ),
      pickerInput(
        "pitch_types", "Pitch Categories",
        choices  = c("Fastball", "Breaking Ball", "Offspeed"),
        selected = c("Fastball", "Breaking Ball", "Offspeed"),
        multiple = TRUE,
        options  = list(`actions-box` = TRUE)
      ),
      selectInput("count", "Count",
        choices = c("All", "Pitcher's Count", "Hitter's Count", "2K")
      ),
      sliderInput("innings", "Innings", min = 1, max = 9,
                  value = c(1, 9), step = 1),
      hr(),
      div(
        style = "margin-top:4px;",
        actionButton("sync_data", "⟳ Sync Data from Drive",
                     class = "btn-sm btn-outline-primary w-100"),
        uiOutput("sync_status")
      )
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
              fluidRow(column(12, DTOutput("table_arsenal"))),
              fluidRow(
                column(6, plotlyOutput("plot_zone13",  height = "440px")),
                column(6, plotlyOutput("plot_movement", height = "440px"))
              ),
              fluidRow(column(12, DTOutput("table_pitchers")))
            ),
            tabPanel(
              "Detail",
              fluidRow(column(12, plotlyOutput("plot_density_cards", height = "600px"))),
              fluidRow(column(6,  plotlyOutput("plot_arsenal",       height = "380px")))
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
              fluidRow(column(12, plotlyOutput("plot_swing_zones",    height = "480px"))),
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
    style = "max-width:900px; margin:0 auto; padding:16px;",
    uiOutput("player_ui")
  )
}
