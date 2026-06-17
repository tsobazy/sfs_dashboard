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
  FourSeamFastBall = "#E63946", Sinker          = "#F4A261",
  ChangeUp         = "#2A9D8F", Curveball       = "#457B9D",
  Slider           = "#6A4C93", Sweeper         = "#9B2226",
  Cutter           = "#E9C46A", Splitter        = "#264653",
  Fastball         = "#E76F51", TwoSeamFastBall = "#F4D35E",
  Undefined        = "#AAAAAA"
)

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
  theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    panel.grid.major  = element_line(color = "#e8e8e8"),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(face = "bold", size = 14, color = "#0a1628"),
    plot.subtitle     = element_text(size = 11, color = "#555555"),
    axis.text         = element_text(color = "#333333"),
    legend.background = element_rect(fill = "white"),
    strip.background  = element_rect(fill = "#f0f0f0")
  )
}

plotly_white <- function(p) {
  p %>% layout(
    paper_bgcolor = "white",
    plot_bgcolor  = "white",
    font          = list(color = "#0a1628")
  )
}

source("roster.R")

# ── Layout helpers (used by server.R renderUI) ─────────────────────────────────

coach_sidebar <- function() {
  pt <- sort(unique(data$TaggedPitchType[data$TaggedPitchType != "Undefined"]))
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
      choices = c("All Players", sort(unique(data$Pitcher))),
      options = list(`live-search` = TRUE)
    ),
    dateRangeInput("dates", "Date Range",
      start = min(data$Date, na.rm = TRUE),
      end   = max(data$Date, na.rm = TRUE)
    ),
    pickerInput(
      "pitch_types", "Pitch Types",
      choices = pt, selected = pt, multiple = TRUE,
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
          uiOutput("coach_pitch_glance"),
          tabsetPanel(
            id = "pitch_sub_tabs",
            tabPanel(
              "Overview",
              fluidRow(
                column(6, plotlyOutput("plot_zone",    height = "380px")),
                column(6, plotlyOutput("plot_arsenal", height = "380px"))
              ),
              fluidRow(column(12, DTOutput("table_pitchers")))
            ),
            tabPanel(
              "Detail",
              fluidRow(
                column(12, plotlyOutput("plot_velo_spin", height = "340px"))
              ),
              fluidRow(
                column(6, plotlyOutput("plot_movement", height = "380px")),
                column(6, plotlyOutput("plot_release",  height = "380px"))
              ),
              fluidRow(
                column(6, plotlyOutput("plot_outcomes",      height = "360px")),
                column(6, plotlyOutput("plot_count_heatmap", height = "360px"))
              )
            )
          )
        ),
        tabPanel(
          "Hitting",
          uiOutput("coach_hit_glance"),
          tabsetPanel(
            id = "hit_sub_tabs",
            tabPanel(
              "Overview",
              fluidRow(
                column(12, plotlyOutput("plot_spray", height = "420px"))
              ),
              fluidRow(column(12, DTOutput("table_batters")))
            ),
            tabPanel(
              "Detail",
              fluidRow(
                column(12, plotlyOutput("plot_ev_la", height = "420px"))
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
    )
  )
}

player_shell <- function() {
  div(
    style = "max-width:900px; margin:0 auto; padding:16px;",
    uiOutput("player_ui")
  )
}
