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
      ) %>%
      plotly_white()
  })

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

  # ── Chart 4: Pitcher Leaderboard ──────────────────────────────────────────
  output$table_pitchers <- renderDT({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      group_by(Pitcher) %>%
      summarise(
        Pitches   = n(),
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
        order      = list(list(3, "desc"))
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

}
