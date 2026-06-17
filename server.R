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
      choices  = c("All Players", players),
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
