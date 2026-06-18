library(shiny)
library(bslib)
library(shinyWidgets)
library(shinymanager)
library(plotly)
library(DT)

source("roster.R")

# ── App UI ─────────────────────────────────────────────────────────────────────

ui <- secure_app(
  tags_top = tagList(
    tags$div(
      style = "text-align:center; margin-bottom:16px;",
      tags$img(src = "seagulls_logo.png", height = "60px", class = "custom-logo",
               style = "display:block; margin:0 auto 12px auto;"),
      tags$h3("Welcome to Seagulls Analytics",
              style = "color:#015294; font-weight:700; margin:0;")
    )
  ),
  tags_bottom = tagList(
    tags$div(
      style = "text-align:center; margin-top:16px;",
      tags$a(
        href = "https://collegeseagulls.com",
        "← Seagulls Home Page",
        style = "color:#015294; font-size:13px; text-decoration:none;"
      )
    )
  ),
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),
      tags$script(HTML("
        $(document).ready(function() {
          function abbreviatePitchPicker() {
            $('.bootstrap-select button .filter-option-inner-inner').each(function() {
              var txt = $(this).text();
              txt = txt.replace(/Fastball/g, 'Fast')
                       .replace(/Breaking Ball/g, 'Break')
                       .replace(/Offspeed/g, 'Off');
              $(this).text(txt);
            });
          }
          abbreviatePitchPicker();
          $(document).on('changed.bs.select', '.bootstrap-select', function() {
            setTimeout(abbreviatePitchPicker, 10);
          });
        });
      "))
    ),
    theme = bs_theme(
      bg = "#F8F9FC", fg = "#1A202C",
      primary = "#1D4ED8", secondary = "#64748B",
      base_font    = font_google("Inter"),
      heading_font = font_google("Inter"),
      version = 5
    ),
    tags$style(HTML("
      .bslib-full-screen-enter { display: none !important; }
      .shiny-full-screen-enter  { display: none !important; }
      [data-bs-toggle='bslib-full-screen'] { display: none !important; }
    ")),
    tags$head(tags$style(HTML("
      body { margin:0; padding:0; font-family:'Inter',sans-serif; }
      .value-tile {
        background:white; border:1px solid #E2E8F0; border-radius:8px;
        padding:16px; text-align:center; margin-bottom:12px;
      }
      .value-tile .tile-label {
        font-size:11px; color:#64748B; text-transform:uppercase;
        letter-spacing:.5px; margin-bottom:4px;
      }
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
      .btn-one-light:hover {
        background:rgba(255,255,255,.1) !important; color:#fff !important;
      }
      .game-chip-row { overflow-x:auto; white-space:nowrap; padding:4px 0; margin-bottom:8px; }
      .game-chip-row .btn {
        margin-right:4px; border-radius:20px; font-size:11px;
        padding:3px 10px; white-space:nowrap;
      }
    "))),
    uiOutput("main_ui")
  )
)
