library(shiny)
library(bslib)
library(shinyWidgets)
library(shinymanager)
library(plotly)
library(DT)

source("roster.R")

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
      .tile-trend { font-size:11px; color:#888; margin-top:4px; display:block; }
      .btn-one-light {
        background: transparent; color: #cfe0f0 !important;
        border: 1px solid #cfe0f0 !important; font-size: 12px;
      }
      .btn-one-light:hover {
        background: rgba(255,255,255,0.15) !important; color: #fff !important;
      }
    "))),
    uiOutput("main_ui")
  )
)
