library(shiny)
library(shinymanager)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(scales)

source("roster.R")

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

}
