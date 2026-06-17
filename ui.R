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
