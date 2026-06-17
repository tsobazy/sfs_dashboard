library(shiny)
library(shinymanager)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(scales)

source("roster.R")

# Stat tile helper for player scorecard
stat_tile <- function(label, value_str, css_class = "tile-neutral", trend = NULL,
                      tooltip_text = NULL) {
  label_node <- if (!is.null(tooltip_text)) {
    tagList(label, " ",
            tags$abbr(title = tooltip_text,
                      style = "cursor:help; color:#aaa; font-size:10px; text-decoration:none;",
                      "?"))
  } else {
    label
  }
  div(
    class = "value-tile",
    div(class = "tile-label", label_node),
    div(class = paste("tile-value", css_class), value_str),
    if (!is.null(trend)) trend
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

  app_data <- reactiveVal(data)

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
        class   = paste("btn btn-sm", if (active) "btn-dark" else "btn-outline-secondary"),
        onclick = sprintf(
          "Shiny.setInputValue('selected_game_chip','%s',{priority:'event'})", value
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
    sel       <- if (is.null(input$selected_game_chip)) "Season" else input$selected_game_chip
    all_dates <- sort(unique(app_data()$Date), decreasing = TRUE)
    switch(sel,
      "Season" = NULL,
      "Last 5" = head(all_dates, 5L),
      as.Date(sel)
    )
  })

  # ── Auth ──────────────────────────────────────────────────────────────────
  result_auth <- secure_server(
    check_credentials = check_credentials(
      db         = "credentials.sqlite",
      passphrase = "seagulls2026_db"
    )
  )

  user_role <- reactive({
    req(result_auth$user)
    result_auth$role
  })

  user_player_name <- reactive({
    req(result_auth$user)
    result_auth$player_name
  })

  user_player_type <- reactive({
    req(result_auth$user)
    result_auth$player_type
  })

  user_display_name <- reactive({
    req(result_auth$user)
    result_auth$display_name
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
  observeEvent(user_role(), {
    req(user_role() == "coach")
    pt <- sort(unique(app_data()$TaggedPitchType[app_data()$TaggedPitchType != "Undefined"]))
    updatePickerInput(session, "pitch_types", choices = pt, selected = pt)
  }, once = TRUE)

  # Updates player dropdown whenever view mode changes
  observeEvent(input$view_mode, {
    req(user_role() == "coach")
    players <- if (input$view_mode == "Pitching") {
      sort(unique(app_data()$Pitcher))
    } else {
      sort(unique(app_data()$Batter))
    }
    updatePickerInput(session, "player",
      choices  = c("All Players", players),
      selected = "All Players"
    )
  })

  # ── Master filtered reactive (coach view) ─────────────────────────────────
  fdata <- reactive({
    req(user_role() == "coach")
    req(input$pitch_types, input$count, input$innings)

    game_dates <- selected_game_range()
    d <- app_data() %>%
      filter(
        if (!is.null(game_dates)) Date %in% game_dates else TRUE,
        TaggedPitchType %in% input$pitch_types,
        TaggedPitchType != "Undefined",
        Inning >= input$innings[1],
        Inning <= input$innings[2]
      )

    d <- switch(input$count,
      "All"             = d,
      "Pitcher's Count" = d %>% filter(Count %in% PITCHER_COUNTS),
      "Hitter's Count"  = d %>% filter(Count %in% HITTER_COUNTS),
      "2K"              = d %>% filter(Count %in% TWO_K_COUNTS),
      d
    )

    if (!is.null(input$player) && input$player != "All Players") {
      if (input$view_mode == "Pitching") {
        d <- d %>% filter(Pitcher == input$player)
      } else {
        d <- d %>% filter(Batter == input$player)
      }
    }
    d
  })

  group_col <- reactive({
    if (isTRUE(input$pitch_group_mode == "Category")) "PitchCategory" else "TaggedPitchType"
  })
  group_colors <- reactive({
    if (isTRUE(input$pitch_group_mode == "Category")) PITCH_CATEGORY_COLORS else PITCH_COLORS
  })

  # ── Player filtered reactive ───────────────────────────────────────────────
  player_fdata_base <- reactive({
    req(user_role() == "player")
    req(user_player_name())
    ptype <- user_player_type()
    name  <- user_player_name()
    col   <- if (ptype == "pitcher") "Pitcher" else "Batter"
    app_data() %>%
      filter(.data[[col]] == name, TaggedPitchType != "Undefined")
  })

  # ── Chart 1: Strike Zone Map ───────────────────────────────────────────────
  output$plot_zone <- renderPlotly({
    req(nrow(fdata()) > 0)
    home_plate <- data.frame(
      x = c(SZ_LEFT, SZ_RIGHT, SZ_RIGHT, 0, SZ_LEFT),
      y = c(0, 0, -0.25, -0.5, -0.25)
    )
    p <- ggplot(fdata(), aes(
        x = PlateLocSide, y = PlateLocHeight,
        color = TaggedPitchType,
        text = paste0("Pitcher: ", Pitcher,
                      "<br>Type: ", TaggedPitchType,
                      "<br>Speed: ", round(RelSpeed, 1), " mph",
                      "<br>Result: ", PitchCall)
      )) +
      geom_polygon(data = home_plate, aes(x = x, y = y),
                   inherit.aes = FALSE, fill = "#e8e8e8", color = "#888888") +
      annotate("rect",
        xmin = SZ_LEFT, xmax = SZ_RIGHT,
        ymin = SZ_BOT,  ymax = SZ_TOP,
        fill = NA, color = "black", linewidth = 0.8
      ) +
      geom_point(alpha = 0.55, size = 2) +
      scale_color_manual(values = PITCH_COLORS, drop = FALSE) +
      scale_x_continuous(limits = c(-2.5, 2.5)) +
      scale_y_continuous(limits = c(-0.6, 5)) +
      labs(
        title    = "Pitch Location Map",
        subtitle = "Catcher's-eye view — you're looking out toward the pitcher",
        color    = NULL, x = "Horizontal (ft)", y = "Height (ft)"
      ) +
      theme_seagulls()
    plotly_clean(ggplotly(p, tooltip = "text"))
  })

  # ── Chart 2: Pitch Arsenal Donut ──────────────────────────────────────────
  output$plot_arsenal <- renderPlotly({
    req(nrow(fdata()) > 0)
    gcol <- group_col()
    d <- fdata() %>%
      group_by(.data[[gcol]]) %>%
      summarise(
        n        = n(),
        avg_spd  = round(mean(RelSpeed,  na.rm = TRUE), 1),
        avg_spin = round(mean(SpinRate,  na.rm = TRUE), 0),
        .groups  = "drop"
      ) %>%
      mutate(pct = n / sum(n))

    cols <- group_colors()
    plot_ly(d,
      labels = d[[gcol]], values = ~n,
      type   = "pie", hole = 0.5,
      marker = list(colors = unname(cols[d[[gcol]]])),
      text   = ~paste0(d[[gcol]], "<br>", n, " pitches (",
                       scales::percent(pct, accuracy = 1), ")<br>",
                       avg_spd, " mph | ", avg_spin, " rpm"),
      hoverinfo = "text",
      textinfo  = "label+percent"
    ) %>%
      layout(
        title      = list(text = "Pitch Arsenal", font = list(color = "#0a1628")),
        showlegend = FALSE,
        paper_bgcolor = "white", plot_bgcolor = "white",
        font = list(color = "#0a1628")
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── Arsenal overview table — coaches see this first ───────────────────────
  output$table_arsenal <- DT::renderDT({
    req(nrow(fdata()) > 0)
    gcol   <- group_col()
    total  <- nrow(fdata())
    d <- fdata() %>%
      group_by(Pitch = .data[[gcol]]) %>%
      summarise(
        `Usage%`   = scales::percent(n() / total, accuracy = 1),
        `Avg Velo` = round(mean(RelSpeed,           na.rm = TRUE), 1),
        `Max Velo` = round(max(RelSpeed,            na.rm = TRUE), 1),
        `Avg Spin` = round(mean(SpinRate,           na.rm = TRUE), 0),
        `Avg IVB`  = round(mean(InducedVertBreak,   na.rm = TRUE), 1),
        `Avg HB`   = round(mean(HorzBreak,          na.rm = TRUE), 1),
        .groups    = "drop"
      ) %>%
      arrange(desc(as.numeric(sub("%", "", `Usage%`))))

    DT::datatable(d, rownames = FALSE,
      options = list(pageLength = 15, dom = "t", ordering = TRUE),
      class   = "compact stripe"
    ) %>%
      DT::formatStyle("Avg Velo",
        background = DT::styleInterval(c(78, 85), c("#FEF3C7", "white", "#DBEAFE"))
      )
  })

  # ── Chart 3: Velocity & Spin by Pitch Type ────────────────────────────────
  output$plot_velo_spin <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      group_by(TaggedPitchType) %>%
      summarise(
        avg_spd  = mean(RelSpeed,  na.rm = TRUE),
        max_spd  = max(RelSpeed,   na.rm = TRUE),
        avg_spin = mean(SpinRate,  na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      arrange(avg_spd)

    p_spd <- ggplot(d, aes(
        x = avg_spd, y = reorder(TaggedPitchType, avg_spd),
        fill = TaggedPitchType
      )) +
      geom_col(width = 0.6) +
      geom_text(aes(label = round(avg_spd, 1)),
                hjust = 1.1, color = "white", fontface = "bold", size = 3.5) +
      geom_point(aes(x = max_spd), shape = 23, size = 3.5,
                 fill = "white", color = "#0a1628", stroke = 1) +
      geom_text(aes(x = max_spd, label = paste0("Top: ", round(max_spd, 1))),
                hjust = -0.15, color = "#0a1628", fontface = "bold", size = 3.2) +
      scale_fill_manual(values = PITCH_COLORS) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = "Avg Velocity (mph)", y = NULL, title = "Avg Velocity",
           caption = "◊ marker = top velocity") +
      theme_seagulls() + theme(legend.position = "none")

    p_spin <- ggplot(d, aes(
        x = avg_spin, y = reorder(TaggedPitchType, avg_spd),
        fill = TaggedPitchType
      )) +
      geom_col(width = 0.6) +
      geom_text(aes(label = round(avg_spin, 0)),
                hjust = 1.1, color = "white", fontface = "bold", size = 3.5) +
      scale_fill_manual(values = PITCH_COLORS) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Avg Spin Rate (rpm)", y = NULL, title = "Avg Spin Rate") +
      theme_seagulls() + theme(legend.position = "none")

    subplot(
      plotly_clean(ggplotly(p_spd,  tooltip = "none")),
      plotly_clean(ggplotly(p_spin, tooltip = "none")),
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
          c("#fff3e0", "white", "#e0f5f2"))
      ) %>%
      formatStyle("Whiff%",
        background = styleInterval(c(0.19, 0.30),
          c("#fff3e0", "white", "#e0f5f2"))
      )
    dt
  })

  # ── Movement Profile — Savant-style with reference rings ──────────────────
  output$plot_movement <- renderPlotly({
    req(nrow(fdata()) > 0)
    gcol <- group_col()
    d <- fdata() %>%
      group_by(.data[[gcol]]) %>%
      summarise(
        HorzBreak        = mean(HorzBreak,        na.rm = TRUE),
        InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
        n                = n(),
        .groups          = "drop"
      )

    rings <- dplyr::bind_rows(lapply(c(6, 12, 18), ring_df))
    cols  <- group_colors()

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

  # ── Chart 6: Release Point Scatter ────────────────────────────────────────
  output$plot_release <- renderPlotly({
    req(nrow(fdata()) > 0)
    ellipse_data <- fdata() %>%
      group_by(TaggedPitchType) %>%
      filter(n() >= 8) %>%
      ungroup()
    p <- ggplot(fdata(), aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
      geom_point(alpha = 0.5, size = 1.5) +
      { if (nrow(ellipse_data) > 0)
          stat_ellipse(data = ellipse_data, aes(group = TaggedPitchType),
                       level = 0.68, linewidth = 0.8)
      } +
      scale_color_manual(values = PITCH_COLORS) +
      labs(
        title    = "Release Point",
        subtitle = "Tighter clusters = more consistent mechanics",
        x = "Horizontal (ft)", y = "Height (ft)", color = NULL
      ) +
      theme_seagulls() +
      theme(legend.position = if (n_distinct(fdata()$TaggedPitchType) <= 1) "none" else "right")
    plotly_clean(ggplotly(p, tooltip = c("x", "y", "colour")))
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
    plotly_clean(ggplotly(p, tooltip = "text"))
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
    plotly_clean(ggplotly(p, tooltip = c("label", "x", "y")))
  })

  # ── Chart 9: Spray Chart (coach — TaggedHitType + ExitSpeed sizing) ──────────
  output$plot_spray <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      filter(!is.na(Direction), !is.na(Distance),
             TaggedHitType %in% BIP_TYPES) %>%
      mutate(
        spray_x = Distance * sin(Direction * pi / 180),
        spray_y = Distance * cos(Direction * pi / 180),
        ev_label = ifelse(is.na(ExitSpeed), "",
                          paste0("<br>EV: ", round(ExitSpeed, 1), " mph"))
      )
    req(nrow(d) > 0)

    bip_colors <- c(
      GroundBall = "#D97706", FlyBall  = "#2563EB",
      LineDrive  = "#16A34A", Popup    = "#9CA3AF"
    )
    outline <- field_outline_df()

    p <- ggplot(d, aes(
        x = spray_x, y = spray_y, color = TaggedHitType, size = ExitSpeed,
        text = paste0(Batter, "<br>", TaggedHitType, "<br>",
                      round(Distance, 0), " ft", ev_label)
      )) +
      geom_path(data = outline, aes(x = x, y = y), inherit.aes = FALSE,
                color = "gray70", linewidth = 0.5) +
      geom_point(alpha = 0.72) +
      scale_color_manual(values = bip_colors) +
      scale_size_continuous(range = c(1.5, 5), guide = "none") +
      coord_fixed(xlim = c(-350, 350), ylim = c(0, 430)) +
      labs(title = "Batted Ball Chart", subtitle = "Size = Exit Velocity",
           x = NULL, y = NULL, color = NULL) +
      theme_seagulls() +
      theme(axis.text = element_blank(), panel.grid = element_blank())
    plotly_clean(ggplotly(p, tooltip = "text"))
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
        xmin = 10, xmax = 30, ymin = 95, ymax = 120,
        fill = "#2DC653", alpha = 0.15, color = "#2DC653",
        linetype = "dashed", linewidth = 0.4
      ) +
      annotate("text", x = 20, y = 122, label = "Barrel Zone",
               color = "#2DC653", size = 3.5, fontface = "bold") +
      geom_point(alpha = 0.7, size = 2.5) +
      scale_color_manual(values = result_colors) +
      scale_x_continuous(limits = c(-40, 50)) +
      scale_y_continuous(limits = c(40, 125)) +
      labs(
        title = "Exit Velocity & Launch Angle",
        x = "Launch Angle (°)", y = "Exit Velocity (mph)", color = NULL
      ) +
      theme_seagulls()
    plotly_clean(ggplotly(p, tooltip = "text"))
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
          c("#fff3e0", "white", "#e0f5f2"))
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
      summarise(swing_pct = mean(swing), n = n(), .groups = "drop") %>%
      tidyr::complete(
        zone_col = c("Left","Middle","Right"),
        zone_row = c("Low","Mid","High")
      )

    p <- ggplot(d, aes(
        x = zone_col, y = zone_row, fill = swing_pct,
        label = if_else(is.na(swing_pct), "—",
                        scales::percent(swing_pct, accuracy = 1))
      )) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(size = 5, fontface = "bold",
                color = ifelse(is.na(d$swing_pct), "#999999", "#0a1628")) +
      scale_fill_gradient2(
        low = "#457B9D", mid = "#f3f3f3", high = "#E63946",
        midpoint = 0.5, na.value = "#f3f3f3",
        labels = scales::percent_format(), name = "Swing%"
      ) +
      scale_x_discrete(limits = c("Left","Middle","Right")) +
      scale_y_discrete(limits = c("Low","Mid","High")) +
      labs(
        title    = "Swing Rates by Zone",
        subtitle = "Catcher's-eye view",
        x = NULL, y = NULL
      ) +
      theme_seagulls() + theme(panel.grid = element_blank())
    plotly_clean(ggplotly(p, tooltip = c("x","y","label")))
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
    plotly_clean(ggplotly(p, tooltip = c("x","fill","count")))
  })

  # ── Chart 14: Pitch Vulnerability Heatmap ─────────────────────────────────
  output$plot_pitch_vuln <- renderPlotly({
    req(nrow(fdata()) > 0)
    all_types <- sort(unique(fdata()$TaggedPitchType))
    d <- fdata() %>%
      group_by(Batter, TaggedPitchType) %>%
      summarise(
        wp = whiff_pct(PitchCall),
        n  = n(),
        .groups = "drop"
      ) %>%
      tidyr::complete(Batter, TaggedPitchType = all_types) %>%
      mutate(wp = if_else(is.na(n) | n < 5, NA_real_, wp))

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
    plotly_clean(ggplotly(p, tooltip = c("x","y","label")))
  })

  # ── Insight Box ───────────────────────────────────────────────────────────
  # ── Coach: Pitching glance row ─────────────────────────────────────────────
  output$coach_pitch_glance <- renderUI({
    req(nrow(fdata()) > 0)
    d    <- fdata()
    spct <- strike_pct(d$PitchCall)
    wpct <- whiff_pct(d$PitchCall)
    cswp <- csw_pct(d$PitchCall)
    gbp  <- gb_pct(d$TaggedHitType)
    fmt  <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 1)

    best_whiff <- d %>%
      group_by(TaggedPitchType) %>%
      summarise(wp = whiff_pct(PitchCall), n = n(), .groups = "drop") %>%
      filter(n >= 10) %>%
      slice_max(wp, n = 1, with_ties = FALSE)

    sentence <- if (nrow(best_whiff) > 0 && !is.na(best_whiff$wp))
      paste0("Best whiff pitch: ", best_whiff$TaggedPitchType,
             " (", fmt(best_whiff$wp), ")")
    else NULL

    tagList(
      div(
        style = "padding:12px 0 8px;",
        layout_columns(
          col_widths = breakpoints(sm = 6, md = 3),
          stat_tile("Strike%", fmt(spct), tile_class(spct, 0.65, 0.54)),
          stat_tile("Whiff%",  fmt(wpct), tile_class(wpct, 0.30, 0.19)),
          stat_tile("CSW%",    fmt(cswp), tile_class(cswp, 0.28, 0.20)),
          stat_tile("GB%",     fmt(gbp),  tile_class(gbp,  0.45, 0.30))
        ),
        if (!is.null(sentence))
          tags$p(sentence,
                 style = "font-size:12px; color:#555; margin:2px 0 8px;")
      )
    )
  })

  # ── Coach: Hitting glance row ──────────────────────────────────────────────
  output$coach_hit_glance <- renderUI({
    req(nrow(fdata()) > 0)
    d    <- fdata()
    d_ip <- d %>% filter(PitchCall == "InPlay", !is.na(ExitSpeed))
    avg_ev <- mean(d_ip$ExitSpeed, na.rm = TRUE)
    hh     <- hard_hit_pct(d_ip$ExitSpeed)
    brl    <- barrel_pct(d_ip$ExitSpeed, d_ip$Angle)

    team_avgs <- d %>%
      group_by(Batter) %>%
      summarise(
        PA = n_distinct(paste(Date, Inning, PAofInning)),
        H  = sum(PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm = TRUE),
        BB = sum(KorBB == "Walk", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      summarise(total_H = sum(H), total_AB = sum(PA - BB))
    tavg <- if (team_avgs$total_AB > 0) team_avgs$total_H / team_avgs$total_AB else NA_real_

    fmt_ev  <- function(x) if (is.na(x)) "—" else paste0(round(x, 1), " mph")
    fmt_pct <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 1)
    fmt_avg <- function(x) if (is.na(x)) "—" else formatC(x, digits = 3, format = "f")

    div(
      style = "padding:12px 0 8px;",
      layout_columns(
        col_widths = breakpoints(sm = 6, md = 3),
        stat_tile("AVG",       fmt_avg(tavg),  tile_class(tavg,   0.300, 0.230)),
        stat_tile("Hard Hit%", fmt_pct(hh),    tile_class(hh,     0.40,  0.25)),
        stat_tile("Barrel%",   fmt_pct(brl),   tile_class(brl,    0.08,  0.04)),
        stat_tile("Avg EV",    fmt_ev(avg_ev), tile_class(avg_ev, 92,    82))
      )
    )
  })

  # ── Sync status display ────────────────────────────────────────────────────
  output$sync_status <- renderUI({ NULL })

  # ── Google Drive sync handler ──────────────────────────────────────────────
  observeEvent(input$sync_data, {
    req(user_role() == "coach")

    folder_id <- Sys.getenv("SEAGULLS_DRIVE_FOLDER_ID")
    if (nchar(folder_id) == 0) {
      output$sync_status <- renderUI({
        tags$small(
          "Set SEAGULLS_DRIVE_FOLDER_ID in .Renviron and restart the app.",
          style = "color:#F4A261; font-size:11px; display:block; margin-top:4px;"
        )
      })
      return()
    }

    output$sync_status <- renderUI({
      tags$small("Syncing…",
                 style = "color:#555; font-size:11px; display:block; margin-top:4px;")
    })

    tryCatch({
      n_new  <- sync_from_drive(folder_id)
      n_rows <- build_combined_csv()

      new_data <- readr::read_csv("all_fall_25.csv", show_col_types = FALSE)
      new_data$TaggedPitchType <- dplyr::if_else(
        new_data$TaggedPitchType %in% c("Other", NA_character_),
        "Undefined", new_data$TaggedPitchType
      )
      new_data$Count <- paste0(new_data$Balls, "-", new_data$Strikes)
      app_data(new_data)

      pt <- sort(unique(new_data$TaggedPitchType[new_data$TaggedPitchType != "Undefined"]))
      updatePickerInput(session, "pitch_types", choices = pt, selected = pt)
      if (input$view_mode == "Pitching") {
        updatePickerInput(session, "player",
          choices  = c("All Players", sort(unique(new_data$Pitcher))),
          selected = "All Players"
        )
      } else {
        updatePickerInput(session, "player",
          choices  = c("All Players", sort(unique(new_data$Batter))),
          selected = "All Players"
        )
      }

      output$sync_status <- renderUI({
        tags$small(
          paste0("✓ ", n_new, " new file(s) — ",
                 format(n_rows, big.mark = ","), " pitches loaded"),
          style = "color:#2A9D8F; font-size:11px; display:block; margin-top:4px;"
        )
      })
    }, error = function(e) {
      output$sync_status <- renderUI({
        tags$small(
          paste0("Error: ", conditionMessage(e)),
          style = "color:#E63946; font-size:11px; display:block; margin-top:4px;
                  word-break:break-word;"
        )
      })
    })
  })

  # ── Player: game list and selection ───────────────────────────────────────
  player_games <- reactive({
    req(user_role() == "player")
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
    i <- min(game_index(), length(g))
    g[[i]]
  })

  player_fdata <- reactive({
    req(user_role() == "player")
    player_fdata_base() %>%
      filter(Date == selected_game())
  })

  # Last 5 games before the selected game — used for trend baseline
  player_recent_fdata <- reactive({
    req(user_role() == "player")
    all_dates  <- player_fdata_base() %>% pull(Date) %>% unique() %>% sort(decreasing = TRUE)
    sel        <- selected_game()
    past_dates <- head(all_dates[all_dates < sel], 5L)
    if (length(past_dates) == 0L)
      return(player_fdata_base() %>% filter(FALSE))
    player_fdata_base() %>% filter(Date %in% past_dates)
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
    req(user_role() == "player")
    d <- player_fdata()
    if (nrow(d) == 0) return(div("No data for this game.", style = "color:#888; margin:16px 0;"))

    spct <- strike_pct(d$PitchCall)
    wpct <- whiff_pct(d$PitchCall)
    cswp <- csw_pct(d$PitchCall)
    chsp <- chase_pct(d$PlateLocSide, d$PlateLocHeight, d$PitchCall)

    fmt <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 1)

    # Trend baseline from last 5 games
    d_rec      <- player_recent_fdata()
    has_recent <- nrow(d_rec) > 0

    mk_trend <- function(curr, base) {
      if (!has_recent || is.na(curr) || is.na(base))
        return(tags$small("— first game with data", class = "tile-trend"))
      diff <- curr - base
      if (abs(diff) < 0.001)
        return(tags$small("— stable vs last 5 games", class = "tile-trend"))
      dir <- if (diff > 0) "↑" else "↓"
      pts <- round(abs(diff) * 100, 1)
      tags$small(paste0(dir, " ", pts, " pts vs last 5 games"), class = "tile-trend")
    }

    spct_base <- if (has_recent) strike_pct(d_rec$PitchCall) else NA_real_
    wpct_base <- if (has_recent) whiff_pct(d_rec$PitchCall)  else NA_real_
    cswp_base <- if (has_recent) csw_pct(d_rec$PitchCall)    else NA_real_
    chsp_base <- if (has_recent) chase_pct(d_rec$PlateLocSide,
                                           d_rec$PlateLocHeight,
                                           d_rec$PitchCall)   else NA_real_

    # Takeaway sentence
    hi_thr <- c("Strike%" = 0.65, "Whiff%" = 0.30, "CSW%" = 0.28)
    lo_thr <- c("Strike%" = 0.54, "Whiff%" = 0.19, "CSW%" = 0.20)
    vals   <- c("Strike%" = spct,  "Whiff%" = wpct,  "CSW%" = cswp)
    good_m <- names(vals)[!is.na(vals) & vals >= hi_thr]
    weak_m <- names(vals)[!is.na(vals) & vals <= lo_thr]

    first_clause <- if (length(good_m) > 0) {
      paste0("Your ", good_m[1], " was strong at ", fmt(vals[good_m[1]]))
    } else if (length(weak_m) > 0) {
      paste0("Your ", weak_m[1], " is an area to develop (", fmt(vals[weak_m[1]]), ")")
    } else {
      paste0("Strike% ", fmt(spct))
    }

    best_wp <- d %>%
      group_by(TaggedPitchType) %>%
      summarise(wp = whiff_pct(PitchCall), n = n(), .groups = "drop") %>%
      filter(n >= 5) %>%
      slice_max(wp, n = 1, with_ties = FALSE)

    second_clause <- if (nrow(best_wp) > 0 && !is.na(best_wp$wp))
      paste0(" — your ", best_wp$TaggedPitchType,
             " generated the most whiffs (", fmt(best_wp$wp), ").")
    else "."

    tagList(
      tags$h6("Pitching", style = "color:#0a1628; font-weight:600; margin:12px 0 8px;"),
      div(
        style = "background:#f0fafb; border-left:3px solid #2A9D8F; padding:10px 14px;
                 border-radius:4px; margin-bottom:12px; font-size:13px; color:#0a1628;",
        paste0(first_clause, second_clause)
      ),
      layout_columns(
        col_widths = breakpoints(sm = 6, md = 3),
        stat_tile("Strike%", fmt(spct), tile_class(spct, 0.65, 0.54),
                  trend        = mk_trend(spct, spct_base),
                  tooltip_text = "Percent of your pitches that were strikes, swung at, or put in play."),
        stat_tile("Whiff%",  fmt(wpct), tile_class(wpct, 0.30, 0.19),
                  trend        = mk_trend(wpct, wpct_base),
                  tooltip_text = "Share of swings where the hitter completely missed — higher is better for pitchers."),
        stat_tile("CSW%",    fmt(cswp), tile_class(cswp, 0.28, 0.20),
                  trend        = mk_trend(cswp, cswp_base),
                  tooltip_text = "Called Strikes + Whiffs: pitches where the catcher or swing gave you a strike with no contact."),
        stat_tile("Chase%",  fmt(chsp), tile_class(chsp, 0.30, 0.00),
                  trend        = mk_trend(chsp, chsp_base),
                  tooltip_text = "How often hitters swung at pitches outside the strike zone — higher means better stuff.")
      ),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6),
        plotlyOutput("player_zone",     height = "360px"),
        plotlyOutput("player_movement", height = "360px")
      )
    )
  })

  output$player_zone <- renderPlotly({
    d <- player_fdata()
    req(nrow(d) > 0)
    home_plate <- data.frame(
      x = c(SZ_LEFT, SZ_RIGHT, SZ_RIGHT, 0, SZ_LEFT),
      y = c(0, 0, -0.25, -0.5, -0.25)
    )
    p <- ggplot(d, aes(
        x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType,
        text = paste0(TaggedPitchType, "<br>", round(RelSpeed,1), " mph<br>", PitchCall)
      )) +
      geom_polygon(data = home_plate, aes(x = x, y = y),
                   inherit.aes = FALSE, fill = "#e8e8e8", color = "#888888") +
      annotate("rect", xmin=SZ_LEFT, xmax=SZ_RIGHT, ymin=SZ_BOT, ymax=SZ_TOP,
               fill=NA, color="black", linewidth=0.8) +
      geom_point(alpha = 0.6, size = 2.5) +
      scale_color_manual(values = PITCH_COLORS, drop = FALSE) +
      scale_x_continuous(limits = c(-2.5, 2.5)) +
      scale_y_continuous(limits = c(-0.6, 5)) +
      labs(title = "Pitch Location",
           subtitle = "Catcher's-eye view — you're looking out toward the pitcher",
           x = "Horizontal (ft)", y = "Height (ft)", color = NULL) +
      theme_seagulls()
    plotly_clean(ggplotly(p, tooltip = "text"))
  })

  output$player_movement <- renderPlotly({
    d <- player_fdata() %>%
      filter(!is.na(HorzBreak), !is.na(InducedVertBreak))
    req(nrow(d) > 0)
    rings <- bind_rows(lapply(c(6, 12, 18), ring_df))
    p <- ggplot(d, aes(x = HorzBreak, y = InducedVertBreak,
        color = TaggedPitchType,
        text = paste0(TaggedPitchType, "<br>IVB: ", round(InducedVertBreak, 1),
                      " in<br>HB: ", round(HorzBreak, 1), " in")
      )) +
      geom_path(data = rings, aes(x = x, y = y, group = r),
                inherit.aes = FALSE, color = "#E2E8F0", linewidth = 0.4) +
      geom_hline(yintercept = 0, color = "#CBD5E1", linewidth = 0.4) +
      geom_vline(xintercept = 0, color = "#CBD5E1", linewidth = 0.4) +
      geom_point(alpha = 0.7, size = 2.5) +
      scale_color_manual(values = PITCH_COLORS, drop = TRUE) +
      coord_fixed(xlim = c(-24, 24), ylim = c(-24, 24)) +
      labs(title = "Movement Profile",
           subtitle = "Pitcher's-hand view — rings at 6\", 12\", 18\"",
           x = "Horizontal Break (in)", y = "Induced Vert Break (in)",
           color = NULL) +
      theme_seagulls() +
      theme(panel.grid = element_blank())
    plotly_clean(ggplotly(p, tooltip = "text"))
  })

  # ── Player: hitter section ────────────────────────────────────────────────
  output$player_hitter_section <- renderUI({
    req(user_role() == "player")
    d <- player_fdata()
    if (nrow(d) == 0) return(div("No data for this game.", style = "color:#888; margin:16px 0;"))

    d_ip   <- d %>% filter(PitchCall == "InPlay", !is.na(ExitSpeed))
    avg_ev <- mean(d_ip$ExitSpeed, na.rm = TRUE)
    hh     <- hard_hit_pct(d_ip$ExitSpeed)

    d_zone  <- d %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    in_zone <- d_zone$PlateLocSide  >= SZ_LEFT & d_zone$PlateLocSide  <= SZ_RIGHT &
               d_zone$PlateLocHeight >= SZ_BOT  & d_zone$PlateLocHeight <= SZ_TOP
    swings  <- d_zone$PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable",
                                        "FoulBallFieldable","InPlay")
    swing_zone <- if (sum(in_zone) > 0) sum(in_zone & swings) / sum(in_zone) else NA_real_
    ooz        <- !in_zone
    chase      <- if (sum(ooz) > 0) sum(ooz & swings) / sum(ooz) else NA_real_

    fmt_ev  <- function(x) if (is.na(x)) "—" else paste0(round(x, 1), " mph")
    fmt_pct <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 1)

    # Trend baseline from last 5 games
    d_rec      <- player_recent_fdata()
    has_recent <- nrow(d_rec) > 0

    mk_trend <- function(curr, base) {
      if (!has_recent || is.na(curr) || is.na(base))
        return(tags$small("— first game with data", class = "tile-trend"))
      diff <- curr - base
      if (abs(diff) < 0.001)
        return(tags$small("— stable vs last 5 games", class = "tile-trend"))
      dir <- if (diff > 0) "↑" else "↓"
      pts <- round(abs(diff) * 100, 1)
      tags$small(paste0(dir, " ", pts, " pts vs last 5 games"), class = "tile-trend")
    }

    mk_trend_ev <- function(curr, base) {
      if (!has_recent || is.na(curr) || is.na(base))
        return(tags$small("— first game with data", class = "tile-trend"))
      diff <- curr - base
      if (abs(diff) < 0.1)
        return(tags$small("— stable vs last 5 games", class = "tile-trend"))
      dir <- if (diff > 0) "↑" else "↓"
      tags$small(paste0(dir, " ", round(abs(diff), 1), " mph vs last 5 games"),
                 class = "tile-trend")
    }

    d_rec_ip    <- d_rec %>% filter(PitchCall == "InPlay", !is.na(ExitSpeed))
    avg_ev_base <- mean(d_rec_ip$ExitSpeed, na.rm = TRUE)
    hh_base     <- hard_hit_pct(d_rec_ip$ExitSpeed)

    d_rec_zone <- d_rec %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    if (nrow(d_rec_zone) > 0) {
      in_zone_r       <- d_rec_zone$PlateLocSide  >= SZ_LEFT & d_rec_zone$PlateLocSide  <= SZ_RIGHT &
                         d_rec_zone$PlateLocHeight >= SZ_BOT  & d_rec_zone$PlateLocHeight <= SZ_TOP
      swings_r        <- d_rec_zone$PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable",
                                                       "FoulBallFieldable","InPlay")
      swing_zone_base <- if (sum(in_zone_r) > 0) sum(in_zone_r & swings_r) / sum(in_zone_r) else NA_real_
      ooz_r           <- !in_zone_r
      chase_base      <- if (sum(ooz_r) > 0) sum(ooz_r & swings_r) / sum(ooz_r) else NA_real_
    } else {
      swing_zone_base <- NA_real_
      chase_base      <- NA_real_
    }

    # Takeaway sentence
    first_clause <- if (!is.na(hh) && hh >= 0.40) {
      paste0("You were hitting the ball hard — ", fmt_pct(hh), " hard contact rate")
    } else if (!is.na(avg_ev) && avg_ev >= 92) {
      paste0("Strong exit velocity at ", fmt_ev(avg_ev))
    } else if (!is.na(hh)) {
      paste0("Hard Hit% was ", fmt_pct(hh), " this game")
    } else {
      "Limited ball-in-play data for this game"
    }

    second_clause <- if (!is.na(chase)) {
      if (chase <= 0.25)
        paste0(" — good pitch recognition (", fmt_pct(chase), " chase rate).")
      else
        paste0(" — pitches outside the zone drew ", fmt_pct(chase), " of your swings.")
    } else "."

    tagList(
      tags$h6("Hitting", style = "color:#0a1628; font-weight:600; margin:12px 0 8px;"),
      div(
        style = "background:#f0fafb; border-left:3px solid #2A9D8F; padding:10px 14px;
                 border-radius:4px; margin-bottom:12px; font-size:13px; color:#0a1628;",
        paste0(first_clause, second_clause)
      ),
      layout_columns(
        col_widths = breakpoints(sm = 6, md = 3),
        stat_tile("Avg Exit Velo", fmt_ev(avg_ev),     tile_class(avg_ev,     92,   82),
                  trend        = mk_trend_ev(avg_ev, avg_ev_base),
                  tooltip_text = "How hard you hit the ball on average — 95+ mph is considered hard contact."),
        stat_tile("Hard Hit%",     fmt_pct(hh),         tile_class(hh,         0.40, 0.25),
                  trend        = mk_trend(hh, hh_base),
                  tooltip_text = "Share of your batted balls hit at 95 mph or harder."),
        stat_tile("Zone Swing%",   fmt_pct(swing_zone), tile_class(swing_zone, 0.70, 0.00),
                  trend        = mk_trend(swing_zone, swing_zone_base),
                  tooltip_text = "How often you swung at pitches inside the strike zone — attacking hittable pitches."),
        stat_tile("Chase%",        fmt_pct(chase),      tile_class(chase, 0.25, 0.35, hi_good = FALSE),
                  trend        = mk_trend(chase, chase_base),
                  tooltip_text = "How often you swung at pitches outside the strike zone — lower means better pitch recognition.")
      ),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6),
        plotlyOutput("player_spray",       height = "360px"),
        plotlyOutput("player_swing_zones", height = "360px")
      )
    )
  })

  output$player_spray <- renderPlotly({
    d <- player_fdata() %>%
      filter(!is.na(Direction), !is.na(Distance),
             PlayResult %in% c("Single","Double","Triple","HomeRun","Out")) %>%
      mutate(
        spray_x = Distance * sin(Direction * pi / 180),
        spray_y = Distance * cos(Direction * pi / 180),
        ev_label = ifelse(is.na(ExitSpeed), "",
                          paste0("<br>EV: ", round(ExitSpeed, 1), " mph"))
      )
    req(nrow(d) > 0)
    hit_colors <- c(Single = "#2DC653", Double = "#F5C518",
                    Triple = "#FF8C00", HomeRun = "#E63946", Out = "#AAAAAA")
    outline <- field_outline_df()
    p <- ggplot(d, aes(x = spray_x, y = spray_y, color = PlayResult,
        text = paste0(PlayResult, "<br>", round(Distance, 0), " ft", ev_label))) +
      geom_path(data = outline, aes(x = x, y = y), inherit.aes = FALSE,
                color = "gray70", linewidth = 0.5) +
      geom_point(alpha = 0.8, size = 3) +
      scale_color_manual(values = hit_colors) +
      coord_fixed(xlim = c(-350, 350), ylim = c(0, 430)) +
      labs(title = "Where the Ball Was Hit", x = NULL, y = NULL, color = NULL) +
      theme_seagulls() +
      theme(axis.text = element_blank(), panel.grid = element_blank())
    plotly_clean(ggplotly(p, tooltip = "text"))
  })

  output$player_swing_zones <- renderPlotly({
    d <- player_fdata()
    req(nrow(d) > 0)
    zone_w <- (SZ_RIGHT - SZ_LEFT) / 3
    zone_h <- (SZ_TOP   - SZ_BOT)  / 3
    ds <- d %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        zone_col = cut(PlateLocSide,
          breaks = c(-Inf, SZ_LEFT + zone_w, SZ_LEFT + 2 * zone_w, Inf),
          labels = c("Left","Middle","Right")),
        zone_row = cut(PlateLocHeight,
          breaks = c(-Inf, SZ_BOT + zone_h, SZ_BOT + 2 * zone_h, Inf),
          labels = c("Low","Mid","High")),
        swing = PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable",
                                  "FoulBallFieldable","InPlay")
      ) %>%
      filter(!is.na(zone_col), !is.na(zone_row)) %>%
      group_by(zone_col, zone_row) %>%
      summarise(swing_pct = mean(swing), n = n(), .groups = "drop") %>%
      tidyr::complete(
        zone_col = c("Left","Middle","Right"),
        zone_row = c("Low","Mid","High")
      )
    p <- ggplot(ds, aes(x = zone_col, y = zone_row, fill = swing_pct,
        label = if_else(is.na(swing_pct), "—",
                        scales::percent(swing_pct, accuracy = 1)))) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(size = 5, fontface = "bold",
                color = ifelse(is.na(ds$swing_pct), "#999999", "#0a1628")) +
      scale_fill_gradient2(low = "#457B9D", mid = "#f3f3f3", high = "#E63946",
        midpoint = 0.5, na.value = "#f3f3f3",
        labels = scales::percent_format(), name = "Swing%") +
      scale_x_discrete(limits = c("Left","Middle","Right")) +
      scale_y_discrete(limits = c("Low","Mid","High")) +
      labs(title = "Swing Rates by Zone",
           subtitle = "Catcher's-eye view", x = NULL, y = NULL) +
      theme_seagulls() + theme(panel.grid = element_blank())
    plotly_clean(ggplotly(p, tooltip = c("x","y","label")))
  })


}
