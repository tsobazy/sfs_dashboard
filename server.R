library(shiny)
library(shinymanager)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(scales)

options(shiny.reactlog = FALSE)

source("roster.R")

# Stat tile helper for player scorecard
stat_tile <- function(label, value_str, css_class = "tile-neutral", trend = NULL,
                      tooltip_text = NULL) {
  div(
    class = "value-tile",
    title = tooltip_text,
    div(class = "tile-label", label),
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

# Badge appended to a tile label: ★ = exceeds threshold, "focus" = below floor
metric_badge <- function(val, hi_thr, lo_thr, hi_good = TRUE) {
  if (is.na(val)) return(NULL)
  good <- if (hi_good) val >= hi_thr else val <= hi_thr
  bad  <- if (hi_good) val <= lo_thr else val >= lo_thr
  if (good) tags$span("★", style = "color:#2A9D8F; margin-left:4px; font-size:10px;")
  else if (bad) tags$span("focus", style = "color:#F4A261; margin-left:3px; font-size:9px; font-weight:700; letter-spacing:.3px;")
  else NULL
}

format_pitch_result <- function(pitch_call, play_result) {
  dplyr::case_when(
    pitch_call == "StrikeSwinging"                                  ~ "Swing & Miss",
    pitch_call == "StrikeCalled"                                    ~ "Called Strike",
    pitch_call %in% c("FoulBallNotFieldable","FoulBallFieldable")   ~ "Foul",
    pitch_call == "Ball"                                            ~ "Ball",
    pitch_call == "HitByPitch"                                      ~ "HBP",
    pitch_call == "InPlay" & play_result == "Single"                ~ "1B",
    pitch_call == "InPlay" & play_result == "Double"                ~ "2B",
    pitch_call == "InPlay" & play_result == "Triple"                ~ "3B",
    pitch_call == "InPlay" & play_result == "HomeRun"               ~ "HR",
    pitch_call == "InPlay" & play_result == "Out"                   ~ "Out",
    pitch_call == "InPlay"                                          ~ "In Play",
    TRUE                                                            ~ pitch_call
  )
}

server <- function(input, output, session) {

  app_data <- reactiveVal(data)

  # ── Season / game window reactives ────────────────────────────────────────
  season_games <- reactive({
    req(input$season)
    app_data() %>%
      filter(Season == input$season) %>%
      pull(Date) %>% unique() %>% sort(decreasing = TRUE)
  })

  observeEvent(season_games(), {
    req(user_role() == "coach")
    g      <- season_games()
    labels <- format(g, "%b %d, %Y")
    updatePickerInput(session, "custom_games",
      choices  = setNames(as.character(g), labels),
      selected = character(0)
    )
  }, ignoreNULL = TRUE)

  selected_games <- reactive({
    req(input$season, input$game_window)
    g <- season_games()
    switch(input$game_window,
      "Last 5"      = head(g, 5L),
      "Last 10"     = head(g, 10L),
      "Full Season" = g,
      "Custom"      = { req(input$custom_games); as.Date(input$custom_games) },
      g
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
    paste0(user_display_name(), " (Coach)")
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
      div(style = "margin:0; padding:0;", uiOutput("player_ui"))
    }
  })

  # ── Data-quality note (auto-cleaning summary) ─────────────────────────────
  clean_summary_rv <- reactiveVal(
    if (exists("DATA_CLEAN_SUMMARY")) DATA_CLEAN_SUMMARY else NULL
  )

  output$data_quality_note <- renderUI({
    s <- clean_summary_rv()
    if (is.null(s)) return(NULL)
    total <- sum(s)
    if (total == 0)
      return(div(style = "color:#94A3B8; font-size:11px; margin-top:4px;",
                 "✓ Data verified — no errors found."))
    breakdown <- paste(sprintf("%s: %d", names(s)[s > 0], s[s > 0]), collapse = "\n")
    div(
      style = "margin-top:6px; padding:8px 10px; background:#0e1f33;
               border:1px solid rgba(255,255,255,0.12); border-radius:8px;",
      title = breakdown,  # hover shows the per-type breakdown
      div(style = "color:#2A9D8F; font-size:12px; font-weight:600;",
          sprintf("✓ %d data errors auto-cleaned", total)),
      div(style = "color:#94A3B8; font-size:10px; margin-top:2px;",
          "Bad TrackMan readings removed before charts. Hover for details.")
    )
  })

  # pitch_types choices are static categories — no dynamic init needed

  # Updates player dropdown whenever main tab changes
  observeEvent(input$main_tabs, {
    req(user_role() == "coach")
    d <- app_data()
    players <- if (input$main_tabs == "Pitching") {
      sort(unique(d$Pitcher[d$PitcherTeam %in% SEAGULLS_TEAM]))
    } else {
      sort(unique(d$Batter[d$BatterTeam %in% SEAGULLS_TEAM]))
    }
    updatePickerInput(session, "player",
      choices  = c("All Players", players),
      selected = "All Players"
    )
  })

  # ── Master filtered reactive (coach view) ─────────────────────────────────
  fdata <- reactive({
    req(user_role() == "coach")
    req(input$pitch_cats, input$count, input$innings)

    game_dates <- selected_games()
    d <- app_data() %>%
      filter(
        Date %in% game_dates,
        PitchCategory %in% input$pitch_cats,
        PitchCategory != "Undefined",
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

    # Restrict to Seagulls players: our pitchers on Pitching, our hitters on
    # Hitting. Keeps "All Players" aggregates and leaderboards opponent-free.
    if (is.null(input$main_tabs) || input$main_tabs == "Pitching") {
      d <- d %>% filter(PitcherTeam %in% SEAGULLS_TEAM)
      if (!is.null(input$player) && input$player != "All Players")
        d <- d %>% filter(Pitcher == input$player)
    } else {
      d <- d %>% filter(BatterTeam %in% SEAGULLS_TEAM)
      if (!is.null(input$player) && input$player != "All Players")
        d <- d %>% filter(Batter == input$player)
    }
    d
  })

  # ── Spray click filter (coach hitting tab) ────────────────────────────────
  spray_click_filter <- reactiveVal(NULL)

  observeEvent(list(input$player, input$game_window, input$main_tabs), {
    spray_click_filter(NULL)
  })

  observeEvent(event_data("plotly_click", source = "spray_chart"), {
    click <- event_data("plotly_click", source = "spray_chart")
    if (!is.null(click$customdata)) {
      clicked <- click$customdata[[1]]
      if (identical(spray_click_filter(), clicked)) {
        spray_click_filter(NULL)
      } else {
        spray_click_filter(clicked)
      }
    }
  })

  observeEvent(event_data("plotly_doubleclick", source = "spray_chart"), {
    spray_click_filter(NULL)
  })

  fdata_hitting <- reactive({
    d <- fdata()
    if (!is.null(spray_click_filter()))
      d <- d %>% filter(PlayResult == spray_click_filter())
    d
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

  # ── Density grid helper — one cell per pitch category × count situation ──────
  render_density_plot <- function(data_reactive, category, count_filter = NULL) {
    renderPlotly({
      d <- data_reactive()
      req(nrow(d) > 0)

      d <- d %>%
        filter(PitchCategory == category,
               !is.na(PlateLocSide), !is.na(PlateLocHeight))

      if (!is.null(count_filter)) {
        d <- switch(count_filter,
          "first"   = d %>% filter(Balls == 0, Strikes == 0),
          "hitter"  = d %>% filter(Count %in% HITTER_COUNTS),
          "pitcher" = d %>% filter(Count %in% PITCHER_COUNTS),
          "2k"      = d %>% filter(Count %in% TWO_K_COUNTS),
          d
        )
      }

      if (nrow(d) < 10) return(plotly_empty())

      home_plate <- data.frame(
        x = c(-0.83,  0.83,  0.83,  0.00, -0.83),
        y = c( 0.00,  0.00, -0.25, -0.50, -0.25)
      )

      p <- ggplot(d, aes(x = PlateLocSide, y = PlateLocHeight)) +
        stat_density_2d(
          aes(fill = after_stat(level)),
          geom    = "polygon",
          contour = TRUE,
          bins    = 8,
          alpha   = 0.85
        ) +
        scale_fill_gradient(low = "#aec9e8", high = "#C0392B", guide = "none") +
        geom_polygon(data = home_plate, aes(x = x, y = y),
                     inherit.aes = FALSE,
                     fill = "#cccccc", color = "#888888") +
        annotate("rect",
                 xmin = SZ_LEFT, xmax = SZ_RIGHT,
                 ymin = SZ_BOT,  ymax = SZ_TOP,
                 fill = NA, color = "#333333",
                 linetype = "dashed", linewidth = 0.7) +
        geom_hline(yintercept = (SZ_BOT + SZ_TOP) / 2,
                   color = "gray70", linetype = "dashed", linewidth = 0.4) +
        coord_fixed(ratio = 1,
                    xlim  = c(-2.5, 2.5),
                    ylim  = c(-0.6, 5.0)) +
        theme_void() +
        theme(
          plot.background  = element_rect(fill = "#eef3f8", color = NA),
          panel.background = element_rect(fill = "#eef3f8", color = NA)
        )

      ggplotly(p, tooltip = character(0)) %>%
        layout(
          yaxis = list(scaleanchor = "x", scaleratio = 1,
                       range = c(-0.6, 5.0), visible = FALSE),
          xaxis = list(range = c(-2.5, 2.5), visible = FALSE),
          annotations = list(list(
            text      = paste0(nrow(d), " pitches"),
            x = 0.5, y = 1.02,
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = 10, color = "#555555")
          )),
          margin        = list(t = 20, b = 5, l = 5, r = 5),
          paper_bgcolor = "#eef3f8",
          plot_bgcolor  = "#eef3f8"
        ) %>%
        config(displayModeBar = FALSE)
    })
  }

  # Overall
  output$density_fb_overall  <- render_density_plot(fdata, "Fastball",      NULL)
  output$density_bb_overall  <- render_density_plot(fdata, "Breaking Ball", NULL)
  output$density_os_overall  <- render_density_plot(fdata, "Offspeed",      NULL)
  # First Pitch
  output$density_fb_first    <- render_density_plot(fdata, "Fastball",      "first")
  output$density_bb_first    <- render_density_plot(fdata, "Breaking Ball", "first")
  output$density_os_first    <- render_density_plot(fdata, "Offspeed",      "first")
  # Hitter's Count
  output$density_fb_hitter   <- render_density_plot(fdata, "Fastball",      "hitter")
  output$density_bb_hitter   <- render_density_plot(fdata, "Breaking Ball", "hitter")
  output$density_os_hitter   <- render_density_plot(fdata, "Offspeed",      "hitter")
  # Pitcher's Count
  output$density_fb_pitcher  <- render_density_plot(fdata, "Fastball",      "pitcher")
  output$density_bb_pitcher  <- render_density_plot(fdata, "Breaking Ball", "pitcher")
  output$density_os_pitcher  <- render_density_plot(fdata, "Offspeed",      "pitcher")
  # Two Strikes
  output$density_fb_2k       <- render_density_plot(fdata, "Fastball",      "2k")
  output$density_bb_2k       <- render_density_plot(fdata, "Breaking Ball", "2k")
  output$density_os_2k       <- render_density_plot(fdata, "Offspeed",      "2k")


  # ── Arsenal overview table — coaches see this first ───────────────────────
  output$table_arsenal <- DT::renderDT({
    req(nrow(fdata()) > 0)
    total <- nrow(fdata())
    d <- fdata() %>%
      group_by(Pitch = PitchCategory) %>%
      summarise(
        `Usage%`   = scales::percent(n() / total, accuracy = 1),
        `Avg Velo` = round(mean(RelSpeed,           na.rm = TRUE), 1),
        `Max Velo` = round(max(RelSpeed,            na.rm = TRUE), 1),
        `Avg Spin` = round(mean(SpinRate,           na.rm = TRUE), 0),
        `Avg IVB`  = round(mean(InducedVertBreak,   na.rm = TRUE), 1),
        `Avg HB`   = round(mean(HorzBreak,          na.rm = TRUE), 1),
        `Whiff%`   = whiff_pct(PitchCall),
        .groups    = "drop"
      ) %>%
      arrange(desc(as.numeric(sub("%", "", `Usage%`))))

    d <- d %>%
      mutate(Pitch = paste0(
        '<span style="display:inline-block;width:10px;height:10px;border-radius:50%;',
        'vertical-align:middle;margin-right:7px;background:',
        vapply(as.character(Pitch), function(x) {
          col <- PITCH_CATEGORY_COLORS[[x]]
          if (is.null(col) || is.na(col)) "#AAAAAA" else col
        }, character(1)),
        ';"></span>', Pitch
      ))

    DT::datatable(d, rownames = FALSE, escape = FALSE,
      options = list(pageLength = 15, dom = "t", ordering = TRUE),
      class   = "compact stripe"
    ) %>%
      DT::formatRound(c("Avg Velo", "Max Velo", "Avg IVB", "Avg HB"), digits = 1) %>%
      DT::formatRound("Avg Spin", digits = 0, mark = ",") %>%
      DT::formatPercentage("Whiff%", digits = 1) %>%
      DT::formatStyle(columns = names(d), textAlign = "right")
  })

  # ── Zone Profile (13-zone) ────────────────────────────────────────────────
  output$plot_zone13 <- renderPlotly({
    req(nrow(fdata()) > 0)

    if (is.null(input$player) || input$player == "All Players") {
      return(
        plot_ly() %>%
          add_annotations(
            text      = "Select a pitcher to see their zone tendencies",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 13, color = "#64748B")
          ) %>%
          layout(paper_bgcolor = "white", plot_bgcolor = "white",
                 xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)) %>%
          config(displayModeBar = FALSE)
      )
    }

    # --- Classify pitches into zones 1-9 (inner) and 11-14 (outer) ---
    zw <- (SZ_RIGHT - SZ_LEFT) / 3
    zh <- (SZ_TOP   - SZ_BOT)  / 3

    d_raw <- fdata() %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        in_col = cut(PlateLocSide,
          breaks = c(SZ_LEFT, SZ_LEFT+zw, SZ_LEFT+2*zw, SZ_RIGHT),
          labels = c("L","M","R"), include.lowest = TRUE),
        in_row = cut(PlateLocHeight,
          breaks = c(SZ_BOT, SZ_BOT+zh, SZ_BOT+2*zh, SZ_TOP),
          labels = c("Low","Mid","High"), include.lowest = TRUE),
        in_zone = !is.na(in_col) & !is.na(in_row),
        near_zone = PlateLocSide  >= SZ_LEFT - zw &
                    PlateLocSide  <= SZ_RIGHT + zw &
                    PlateLocHeight >= SZ_BOT - zh &
                    PlateLocHeight <= SZ_TOP + zh,
        zone = case_when(
          in_zone & in_col=="L" & in_row=="High" ~ "1",
          in_zone & in_col=="M" & in_row=="High" ~ "2",
          in_zone & in_col=="R" & in_row=="High" ~ "3",
          in_zone & in_col=="L" & in_row=="Mid"  ~ "4",       in_zone & in_col=="M" & in_row=="Mid"  ~ "5",
          in_zone & in_col=="R" & in_row=="Mid"  ~ "6",
          in_zone & in_col=="L" & in_row=="Low"  ~ "7",
          in_zone & in_col=="M" & in_row=="Low"  ~ "8",
          in_zone & in_col=="R" & in_row=="Low"  ~ "9",
          !in_zone & near_zone & PlateLocSide <  0 & PlateLocHeight >= (SZ_BOT+SZ_TOP)/2 ~ "11",
          !in_zone & near_zone & PlateLocSide >= 0 & PlateLocHeight >= (SZ_BOT+SZ_TOP)/2 ~ "12",
          !in_zone & near_zone & PlateLocSide <  0 & PlateLocHeight <  (SZ_BOT+SZ_TOP)/2 ~ "13",
          !in_zone & near_zone & PlateLocSide >= 0 & PlateLocHeight <  (SZ_BOT+SZ_TOP)/2 ~ "14",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(zone)) %>%
      count(zone) %>%
      mutate(pct = n / sum(n))

    # Fill in any missing zones with 0
    all_zones <- tibble(zone = as.character(c(1:9, 11:14)))
    d_raw <- left_join(all_zones, d_raw, by = "zone") %>%
      mutate(pct = replace_na(pct, 0), n = replace_na(n, 0))

    pct_lookup <- setNames(d_raw$pct, d_raw$zone)

    # --- Build rectangle data frame for drawing ---
    # Coordinate system: inner grid spans x[-1.5,1.5], y[-1.5,1.5] # Outer margin = 1 unit on every side → outer boundary x[-2.5,2.5], y[-2.5,2.5]

    inner_cells <- tribble(
      ~zone, ~xmin, ~xmax, ~ymin, ~ymax, ~lx,  ~ly,
      "1",  -1.5,  -0.5,   0.5,   1.5,  -1.0,  1.0,
      "2",  -0.5,   0.5,   0.5,   1.5,   0.0,  1.0,
      "3",   0.5,   1.5,   0.5,   1.5,   1.0,  1.0,
      "4",  -1.5,  -0.5,  -0.5,   0.5,  -1.0,  0.0,
      "5",  -0.5,   0.5,  -0.5,   0.5,   0.0,  0.0,
      "6",   0.5,   1.5,  -0.5,   0.5,   1.0,  0.0,
      "7",  -1.5,  -0.5,  -1.5,  -0.5,  -1.0, -1.0,
      "8",  -0.5,   0.5,  -1.5,  -0.5,   0.0, -1.0,
      "9",   0.5,   1.5,  -1.5,  -0.5,   1.0, -1.0
    )

    # Outer zones drawn as quadrants first (inner grid draws on top)
    outer_cells <- tribble(
      ~zone, ~xmin, ~xmax, ~ymin, ~ymax, ~lx,  ~ly,
      "11", -2.5,   0.0,   0.0,   2.5,  -2.0,  2.0,
      "12",  0.0,   2.5,   0.0,   2.5,   2.0,  2.0,
      "13", -2.5,   0.0,  -2.5,   0.0,  -2.0, -2.0,
      "14",  0.0,   2.5,  -2.5,   0.0,   2.0, -2.0
    )

    add_pct <- function(df) {
      df %>% mutate(
        pct_val  = pct_lookup[zone],
        pct_lab  = if_else(is.na(pct_val) | pct_val == 0, "—",
                           paste0(round(pct_val * 100), "%")),
        txt_col  = ifelse(!is.na(pct_val) & pct_val >= 0.15, "white", "#1a1a2e")
      )
    }

    outer_cells <- add_pct(outer_cells)
    inner_cells <- add_pct(inner_cells)

    fill_scale <- colorRampPalette(c("#ffffff", "#ffffff", "#f5c0b0", "#C0392B"))(100)
    fill_fn    <- function(v) {
      idx <- pmax(1, pmin(100, round((pmax(v - 0.05, 0) / 0.15) * 80) + 1))
      fill_scale[idx]
    }

    p <- ggplot() +
      # Outer zones first (drawn behind inner grid)
      geom_rect(data = outer_cells,
                aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill = fill_fn(outer_cells$pct_val),
                color = "black", linewidth = 1.2) +
      geom_text(data = outer_cells,
                aes(x=lx, y=ly, label=pct_lab, color=txt_col),
                size = 4, fontface = "bold") +
      # Inner grid on top
      geom_rect(data = inner_cells,
                aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill = fill_fn(inner_cells$pct_val),
                color = "black", linewidth = 1.2) +
      geom_text(data = inner_cells,
                aes(x=lx, y=ly, label=pct_lab, color=txt_col),
                size = 4, fontface = "bold") +
      scale_color_identity() +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5), expand = FALSE) +
      labs(title    = "Zone Profile (Catcher's View)",
           subtitle = "Catcher's-eye view — looking out toward the pitcher",
           x = NULL, y = NULL,
           caption  = "Zone from Catcher's Perspective") +
      theme_seagulls() +
      theme(axis.text  = element_blank(), axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 13,
                                      color = "#0a1628", margin = margin(b = 6)))

    ggplotly(p, tooltip = character(0)) %>%
      layout(
        yaxis         = list(scaleanchor = "x", scaleratio = 1,
                             range = c(-2.6, 2.6), visible = FALSE),
        xaxis         = list(range = c(-2.6, 2.6), visible = FALSE),
        margin        = list(t = 40, b = 40, l = 20, r = 20),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      ) %>%
      config(displayModeBar = FALSE) %>%
      style(hoverinfo = "none")
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
        order      = list(list(2, "desc"))
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
      ) %>%
      formatStyle("Pitches",
        target    = "row",
        color     = styleInterval(14, c("#aaaaaa", "inherit")),
        fontStyle = styleInterval(14, c("italic", "normal"))
      )
    dt
  })

  # ── Movement Profile — Savant-style with reference rings ──────────────────
  output$plot_movement <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      group_by(PitchCategory) %>%
      summarise(
        HorzBreak        = round(mean(HorzBreak,        na.rm = TRUE), 2),
        InducedVertBreak = round(mean(InducedVertBreak, na.rm = TRUE), 2),
        n                = n(),
        .groups          = "drop"
      )

    rings <- dplyr::bind_rows(lapply(c(6, 12, 18), ring_df))

    p <- ggplot(d, aes(
        x = HorzBreak, y = InducedVertBreak,
        color = PitchCategory, size = n,
        label = PitchCategory
      )) +
      geom_path(data = rings, aes(x = x, y = y, group = r),
                color = "#D1D5DB", linetype = "dashed", linewidth = 0.4,
                inherit.aes = FALSE) +
      geom_hline(yintercept = 0, color = "#CBD5E1", linewidth = 0.5) +
      geom_vline(xintercept = 0, color = "#CBD5E1", linewidth = 0.5) +
      geom_point(alpha = 0.85) +
      geom_text(aes(label = PitchCategory),
                size = 3.2, hjust = -0.2, vjust = 0.5,
                show.legend = FALSE) +
      scale_color_manual(values = PITCH_CATEGORY_COLORS) +
      scale_size_continuous(range = c(4, 12)) +
      coord_fixed() +
      labs(
        title    = "Movement Profile (Pitcher's View)",
        subtitle = "Pitcher's-eye view — like the TV camera behind the mound",
        x = "Horizontal Break (in)", y = "Induced Vert Break (in)",
        color = NULL, size = "Pitches"
      ) +
      theme_seagulls() +
      theme(legend.position = if (n_distinct(d$PitchCategory) <= 1) "none" else "right")
    plotly_clean(ggplotly(p, tooltip = c("label", "x", "y", "size")))
  })


  # ── Pitch Usage Trend ────────────────────────────────────────────────────
  output$plot_usage_trend <- renderPlotly({
    req(nrow(fdata()) > 0)
    d <- fdata() %>%
      group_by(Date, Category = PitchCategory) %>%
      summarise(n_pitches = n(), whiff = whiff_pct(PitchCall), .groups = "drop") %>%
      group_by(Date) %>%
      mutate(usage_pct = n_pitches / sum(n_pitches)) %>%
      ungroup()

    p <- ggplot(d, aes(
        x = Date, y = usage_pct, color = Category, group = Category,
        text = paste0(
          Category, "<br>", format(Date, "%b %d"),
          "<br>Usage: ", scales::percent(usage_pct, accuracy = 1),
          "<br>Whiff%: ", if_else(is.na(whiff), "—",
                                  scales::percent(whiff, accuracy = 1))
        )
      )) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      scale_color_manual(values = PITCH_CATEGORY_COLORS) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = "Pitch Usage Over Time",
           subtitle = "Hover a point for that game's usage and whiff%",
           x = NULL, y = "Usage%", color = NULL) +
      theme_seagulls()
    plotly_clean(ggplotly(p, tooltip = "text"))
  })

  # ── Chart 9: Spray Chart (coach — PlayResult coloring + ExitSpeed sizing) ────
  output$plot_spray <- renderPlotly({
    req(nrow(fdata()) > 0)

    d <- fdata() %>%
      filter(PlayResult %in% c("Single","Double","Triple","HomeRun","Out"),
             !is.na(Direction), !is.na(Distance)) %>%
      mutate(
        spray_x = Distance * sin(Direction * pi / 180),
        spray_y = Distance * cos(Direction * pi / 180)
      )
    req(nrow(d) > 0)

    result_colors <- c(Single = "#2DC653", Double = "#F5C518",
                       Triple = "#FF8C00", HomeRun = "#E63946", Out = "#AAAAAA")
    active <- spray_click_filter()

    d <- d %>% mutate(
      alpha_val = if (is.null(active)) 0.75
                  else ifelse(PlayResult == active, 0.95, 0.12),
      size_val  = if (is.null(active)) 6
                  else ifelse(PlayResult == active, 8, 4)
    )

    outline <- field_outline_df()

    title_txt <- if (!is.null(active))
      paste0("Batted Ball Chart — ", active,
             "s only  <i>(click again or double-click to clear)</i>")
    else
      "Batted Ball Chart  <i>(click a dot to filter by result type)</i>"

    p <- plot_ly(source = "spray_chart") %>%
      add_trace(
        data = outline, type = "scatter", mode = "lines",
        x = ~x, y = ~y,
        line = list(color = "gray70", width = 1.5),
        hoverinfo = "none", showlegend = FALSE
      )

    for (result in c("HomeRun","Triple","Double","Single","Out")) {
      sub <- d %>% filter(PlayResult == result)
      if (nrow(sub) == 0) next
      p <- p %>% add_trace(
        data       = sub,
        type       = "scatter", mode = "markers",
        x          = ~spray_x, y = ~spray_y,
        name       = result,
        customdata = ~PlayResult,
        marker     = list(
          color   = result_colors[result],
          size    = sub$size_val,
          opacity = sub$alpha_val,
          line    = list(width = 0)
        ),
        text = ~paste0(Batter, "<br>", PlayResult,
                       "<br>EV: ", ifelse(is.na(ExitSpeed), "—",
                                          paste0(round(ExitSpeed, 1), " mph")),
                       "<br>Dist: ", ifelse(is.na(Distance), "—",
                                            paste0(round(Distance), " ft"))),
        hoverinfo = "text"
      )
    }

    p %>% layout(
      title         = list(text = title_txt, font = list(size = 13), x = 0),
      xaxis         = list(visible = FALSE, range = c(-350, 350)),
      yaxis         = list(visible = FALSE, range = c(-30, 430),
                           scaleanchor = "x", scaleratio = 1),
      showlegend    = TRUE,
      legend        = list(orientation = "v"),
      paper_bgcolor = "white", plot_bgcolor = "white"
    ) %>%
      config(displayModeBar = FALSE)
  })


  # ── Chart 11: Batter Leaderboard ──────────────────────────────────────────
  output$table_batters <- renderDT({
    req(nrow(fdata_hitting()) > 0)
    d <- fdata_hitting() %>%
      group_by(Batter) %>%
      summarise(
        PA     = n_distinct(paste(Date, Inning, PAofInning)),
        H      = sum(PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm=TRUE),
        BB     = sum(KorBB == "Walk",      na.rm = TRUE),
        HBP    = sum(PitchCall == "HitByPitch", na.rm = TRUE),
        SAC    = sum(PlayResult == "Sacrifice",  na.rm = TRUE),
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
        AB    = PA - BB - HBP - SAC,
        TB    = Single + 2*Double + 3*Triple + 4*HR,
        AVG   = if_else(AB > 0, H / AB, NA_real_),
        OBP   = if_else(PA > 0, (H + BB + HBP) / PA, NA_real_),
        SLG   = if_else(AB > 0, TB / AB, NA_real_),
        `K%`  = K  / PA,
        `BB%` = BB / PA,
        `BB/K` = if_else(K > 0, round(BB / K, 2), NA_real_)
      ) %>%
      left_join(roster_positions[, c("player_name","position")],
                by = c("Batter" = "player_name")) %>%
      select(Batter, Position = position, PA, AVG, OBP, SLG,
             `K%`, `BB%`, `BB/K`, `Avg EV` = avg_ev,
             `Hard Hit%` = hh_pct, `Barrel%` = brl_pct)

    datatable(d,
      options  = list(pageLength = 10, order = list(list(9, "desc"))),
      rownames = FALSE
    ) %>%
      formatRound(c("AVG","OBP","SLG"), digits = 3) %>%
      formatRound("BB/K", digits = 2) %>%
      formatPercentage(c("K%","BB%","Hard Hit%","Barrel%"), digits = 1) %>%
      formatRound("Avg EV", digits = 1) %>%
      formatStyle("PA",
        target    = "row",
        color     = styleInterval(14, c("#aaaaaa", "inherit")),
        fontStyle = styleInterval(14, c("italic", "normal"))
      ) %>%
      formatStyle("Avg EV",
        background = styleInterval(c(82, 92),
          c("#fff3e0", "white", "#e0f5f2"))
      )
  })

  # ── Plate Discipline by Situation ─────────────────────────────────────────
  output$table_plate_discipline <- DT::renderDT({
    req(nrow(fdata()) > 0)

    d <- fdata() %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        in_zone   = PlateLocSide   >= SZ_LEFT & PlateLocSide   <= SZ_RIGHT &
                    PlateLocHeight >= SZ_BOT  & PlateLocHeight <= SZ_TOP,
        swing     = PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable",
                                     "FoulBallFieldable","InPlay"),
        whiff     = PitchCall == "StrikeSwinging",
        chase     = !in_zone & swing,
        situation = case_when(
          Balls == 0 & Strikes == 0  ~ "First Pitch",
          Count %in% HITTER_COUNTS   ~ "Hitter's Count",
          Count %in% PITCHER_COUNTS  ~ "Pitcher's Count",
          Count %in% TWO_K_COUNTS    ~ "2 Strikes",
          TRUE                       ~ NA_character_
        )
      ) %>%
      filter(!is.na(situation)) %>%
      group_by(Situation = situation) %>%
      summarise(
        Pitches  = n(),
        `Zone%`  = paste0(round(mean(in_zone, na.rm = TRUE) * 100), "%"),
        `Swing%` = paste0(round(mean(swing,   na.rm = TRUE) * 100), "%"),
        `Chase%` = paste0(round(
                     sum(chase, na.rm = TRUE) /
                     pmax(sum(!in_zone, na.rm = TRUE), 1) * 100), "%"),
        `Whiff%` = paste0(round(
                     sum(whiff, na.rm = TRUE) /
                     pmax(sum(swing,  na.rm = TRUE), 1) * 100), "%"),
        .groups  = "drop"
      ) %>%
      mutate(Situation = factor(Situation,
        levels = c("First Pitch","Hitter's Count","Pitcher's Count","2 Strikes"))) %>%
      arrange(Situation)

    DT::datatable(d, rownames = FALSE,
      options = list(
        dom        = "t",
        ordering   = FALSE,
        pageLength = 10,
        columnDefs = list(
          list(className = "dt-left",   targets = 0),
          list(className = "dt-center", targets = "_all")
        )
      ),
      class = "compact stripe"
    ) %>%
      DT::formatStyle("Situation",
        fontWeight = "bold", textAlign = "left", fontSize = "14px") %>%
      DT::formatStyle(c("Zone%","Swing%","Chase%","Whiff%"),
        textAlign = "center", fontSize = "13px") %>%
      DT::formatStyle("Pitches",
        textAlign = "center", color = "#999999", fontSize = "12px")
  })

  # ── Chart 12: Swing Rate by Zone (13-zone) ────────────────────────────────
  output$plot_swing_zones <- renderPlotly({
    req(nrow(fdata()) > 0)

    zw <- (SZ_RIGHT - SZ_LEFT) / 3
    zh <- (SZ_TOP   - SZ_BOT)  / 3

    d_raw <- fdata() %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        in_col = cut(PlateLocSide,
          breaks = c(SZ_LEFT, SZ_LEFT+zw, SZ_LEFT+2*zw, SZ_RIGHT),
          labels = c("L","M","R"), include.lowest = TRUE),
        in_row = cut(PlateLocHeight,
          breaks = c(SZ_BOT, SZ_BOT+zh, SZ_BOT+2*zh, SZ_TOP),
          labels = c("Low","Mid","High"), include.lowest = TRUE),
        in_zone   = !is.na(in_col) & !is.na(in_row),
        near_zone = PlateLocSide  >= SZ_LEFT - zw &
                    PlateLocSide  <= SZ_RIGHT + zw &
                    PlateLocHeight >= SZ_BOT - zh &
                    PlateLocHeight <= SZ_TOP + zh,
        swing = PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable",
                                  "FoulBallFieldable","InPlay"),
        zone = case_when(
          in_zone & in_col=="L" & in_row=="High" ~ "1",
          in_zone & in_col=="M" & in_row=="High" ~ "2",
          in_zone & in_col=="R" & in_row=="High" ~ "3",
          in_zone & in_col=="L" & in_row=="Mid"  ~ "4",
          in_zone & in_col=="M" & in_row=="Mid"  ~ "5",
          in_zone & in_col=="R" & in_row=="Mid"  ~ "6",
          in_zone & in_col=="L" & in_row=="Low"  ~ "7",
          in_zone & in_col=="M" & in_row=="Low"  ~ "8",
          in_zone & in_col=="R" & in_row=="Low"  ~ "9",
          !in_zone & near_zone & PlateLocSide <  0 & PlateLocHeight >= (SZ_BOT+SZ_TOP)/2 ~ "11",
          !in_zone & near_zone & PlateLocSide >= 0 & PlateLocHeight >= (SZ_BOT+SZ_TOP)/2 ~ "12",
          !in_zone & near_zone & PlateLocSide <  0 & PlateLocHeight <  (SZ_BOT+SZ_TOP)/2 ~ "13",
          !in_zone & near_zone & PlateLocSide >= 0 & PlateLocHeight <  (SZ_BOT+SZ_TOP)/2 ~ "14",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(zone)) %>%
      group_by(zone) %>%
      summarise(pct_val = mean(swing), n = n(), .groups = "drop")

    all_zones <- tibble(zone = as.character(c(1:9, 11:14)))
    d_raw <- left_join(all_zones, d_raw, by = "zone") %>%
      mutate(pct_val = replace_na(pct_val, 0), n = replace_na(n, 0L))

    pct_lookup <- setNames(d_raw$pct_val, d_raw$zone)

    outer_cells <- tribble(
      ~zone, ~xmin, ~xmax, ~ymin, ~ymax, ~lx,  ~ly,
      "11", -2.5,   0.0,   0.0,   2.5,  -2.0,  2.0,
      "12",  0.0,   2.5,   0.0,   2.5,   2.0,  2.0,
      "13", -2.5,   0.0,  -2.5,   0.0,  -2.0, -2.0,
      "14",  0.0,   2.5,  -2.5,   0.0,   2.0, -2.0
    )

    inner_cells <- tribble(
      ~zone, ~xmin, ~xmax, ~ymin, ~ymax, ~lx,  ~ly,
      "1",  -1.5,  -0.5,   0.5,   1.5,  -1.0,  1.0,
      "2",  -0.5,   0.5,   0.5,   1.5,   0.0,  1.0,
      "3",   0.5,   1.5,   0.5,   1.5,   1.0,  1.0,
      "4",  -1.5,  -0.5,  -0.5,   0.5,  -1.0,  0.0,
      "5",  -0.5,   0.5,  -0.5,   0.5,   0.0,  0.0,
      "6",   0.5,   1.5,  -0.5,   0.5,   1.0,  0.0,
      "7",  -1.5,  -0.5,  -1.5,  -0.5,  -1.0, -1.0,
      "8",  -0.5,   0.5,  -1.5,  -0.5,   0.0, -1.0,
      "9",   0.5,   1.5,  -1.5,  -0.5,   1.0, -1.0
    )

    add_pct <- function(df) {
      df %>% mutate(
        pct_val = pct_lookup[zone],
        pct_lab = if_else(is.na(pct_val) | pct_val == 0, "—",
                          paste0(round(pct_val * 100), "%")),
        txt_col = ifelse(!is.na(pct_val) & pct_val >= 0.55, "white", "#1a1a2e")
      )
    }

    outer_cells <- add_pct(outer_cells)
    inner_cells <- add_pct(inner_cells)

    fill_scale <- colorRampPalette(c("#ffffff", "#f5c0b0", "#C0392B"))(100)
    fill_fn    <- function(v) {
      idx <- pmax(1, pmin(100, round((pmax(v - 0.20, 0) / 0.60) * 99) + 1))
      fill_scale[idx]
    }

    p <- ggplot() +
      geom_rect(data = outer_cells,
                aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill = fill_fn(outer_cells$pct_val),
                color = "black", linewidth = 1.2) +
      geom_text(data = outer_cells,
                aes(x=lx, y=ly, label=pct_lab, color=txt_col),
                size = 4, fontface = "bold") +
      geom_rect(data = inner_cells,
                aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill = fill_fn(inner_cells$pct_val),
                color = "black", linewidth = 1.2) +
      geom_text(data = inner_cells,
                aes(x=lx, y=ly, label=pct_lab, color=txt_col),
                size = 4, fontface = "bold") +
      scale_color_identity() +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5), expand = FALSE) +
      labs(title    = "Swing Rates by Zone (Catcher's View)",
           subtitle = "Catcher's-eye view — looking out toward the pitcher",
           x = NULL, y = NULL,
           caption  = "Zone from Catcher's Perspective") +
      theme_seagulls() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid = element_blank())

    ggplotly(p, tooltip = character(0)) %>%
      layout(
        yaxis         = list(scaleanchor = "x", scaleratio = 1,
                             range = c(-2.6, 2.6), visible = FALSE),
        xaxis         = list(range = c(-2.6, 2.6), visible = FALSE),
        margin        = list(t = 40, b = 40, l = 20, r = 20),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      ) %>%
      config(displayModeBar = FALSE) %>%
      style(hoverinfo = "none")
  })


  # ── Quality Contact Density (Hitting Detail) ──────────────────────────────
  output$plot_quality_contact <- renderPlotly({
    req(nrow(fdata()) > 0)

    blank <- function(msg) {
      plot_ly() %>%
        add_annotations(text = msg, x = 0.5, y = 0.5,
                        xref = "paper", yref = "paper",
                        showarrow = FALSE,
                        font = list(size = 13, color = "#64748B")) %>%
        layout(
          title         = list(text = "Quality Contact Zones",
                               font = list(size = 13, color = "#1A202C")),
          paper_bgcolor = "white", plot_bgcolor = "white",
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
        ) %>%
        config(displayModeBar = FALSE)
    }

    if (is.null(input$player) || input$player == "All Players")
      return(blank("Select a specific batter to see their contact-quality zones."))

    d <- fdata() %>%
      filter(PitchCall == "InPlay",
             !is.na(PlateLocSide), !is.na(PlateLocHeight),
             !is.na(ExitSpeed), ExitSpeed >= 80)

    if (nrow(d) < 10)
      return(blank(paste0("Not enough hard-contact data for ", input$player,
                          " in this sample (need ≥ 10 batted balls at 80+ mph).")))

    hp_x <- c(-0.83,  0.83,  0.83,  0.00, -0.83)
    hp_y <- c( 0.00,  0.00, -0.25, -0.50, -0.25)

    plot_ly() %>%
      add_trace(
        x         = d$PlateLocSide,
        y         = d$PlateLocHeight,
        type      = "histogram2dcontour",
        colorscale = list(c(0, "#EFF6FF"), c(0.4, "#3B82F6"), c(1, "#DC2626")),
        contours  = list(coloring = "fill", showlabels = FALSE),
        ncontours = 10,
        line      = list(width = 0),
        showscale = FALSE,
        hoverinfo = "none"
      ) %>%
      add_shape(
        type = "rect",
        x0 = SZ_LEFT, x1 = SZ_RIGHT, y0 = SZ_BOT, y1 = SZ_TOP,
        line = list(color = "#555555", dash = "dash", width = 1.5),
        fillcolor = "rgba(0,0,0,0)"
      ) %>%
      add_trace(
        x = hp_x, y = hp_y, type = "scatter", mode = "lines",
        fill = "toself", fillcolor = "rgba(210,210,210,0.8)",
        line = list(color = "#999999", width = 1),
        showlegend = FALSE, hoverinfo = "none"
      ) %>%
      layout(
        title = list(
          text = paste0("<b>Quality Contact Zones — ", input$player, "</b><br>",
                        "<sup>Pitch locations of batted balls at 80+ mph ",
                        "(n = ", nrow(d), ")</sup>"),
          font = list(size = 13, color = "#1A202C"), x = 0.05
        ),
        xaxis = list(range = c(-2.5, 2.5), visible = FALSE),
        yaxis = list(range = c(-0.6, 5.0), visible = FALSE,
                     scaleanchor = "x", scaleratio = 1),
        paper_bgcolor = "white", plot_bgcolor = "white",
        margin = list(t = 80, b = 20, l = 20, r = 20)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── Coach: Pitching glance row ─────────────────────────────────────────────
  output$coach_pitch_glance <- renderUI({
    req(nrow(fdata()) > 0)
    d    <- fdata()
    spct <- strike_pct(d$PitchCall)
    wpct <- whiff_pct(d$PitchCall)
    cswp <- csw_pct(d$PitchCall)
    gbp  <- gb_pct(d$TaggedHitType)
    fmt  <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 1)

    tagList(
      div(
        style = "padding:12px 0 8px;",
        layout_columns(
          col_widths = breakpoints(sm = 6, md = 3),
          stat_tile("Strike%", fmt(spct), tile_class(spct, 0.65, 0.53)),
          stat_tile("Whiff%",  fmt(wpct), tile_class(wpct, 0.29, 0.13)),
          stat_tile("CSW%",    fmt(cswp), tile_class(cswp, 0.31, 0.21)),
          stat_tile("GB%",     fmt(gbp),  tile_class(gbp,  0.48, 0.33))
        )
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
        PA  = n_distinct(paste(Date, Inning, PAofInning)),
        H   = sum(PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm = TRUE),
        BB  = sum(KorBB == "Walk", na.rm = TRUE),
        HBP = sum(PitchCall == "HitByPitch", na.rm = TRUE),
        SAC = sum(PlayResult == "Sacrifice",  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      summarise(total_H = sum(H), total_AB = sum(PA - BB - HBP - SAC))
    tavg <- if (team_avgs$total_AB > 0) team_avgs$total_H / team_avgs$total_AB else NA_real_

    fmt_ev  <- function(x) if (is.na(x)) "—" else paste0(round(x, 1), " mph")
    fmt_pct <- function(x) if (is.na(x)) "—" else scales::percent(x, accuracy = 1)
    fmt_avg <- function(x) if (is.na(x)) "—" else formatC(x, digits = 3, format = "f")

    div(
      style = "padding:12px 0 8px;",
      layout_columns(
        col_widths = breakpoints(sm = 6, md = 3),
        stat_tile("AVG",       fmt_avg(tavg),  tile_class(tavg,   0.300, 0.220)),
        stat_tile("Hard Hit%", fmt_pct(hh),    tile_class(hh,     0.50,  0.30)),
        stat_tile("Barrel%",   fmt_pct(brl),   tile_class(brl,    0.12,  0.04)),
        stat_tile("Avg EV",    fmt_ev(avg_ev), tile_class(avg_ev, 84,    73))
      )
    )
  })

  # ── Sync status display ────────────────────────────────────────────────────
  output$sync_status <- renderUI({ NULL })
  output$insights    <- renderUI({ NULL })

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
      new_data <- clean_trackman_data(new_data)
      new_data$TaggedPitchType <- dplyr::if_else(
        new_data$TaggedPitchType %in% c("Other", NA_character_),
        "Undefined", new_data$TaggedPitchType
      )
      new_data$Count        <- paste0(new_data$Balls, "-", new_data$Strikes)
      new_data$PitchCategory <- PITCH_CATEGORY_MAP[new_data$TaggedPitchType]
      new_data$PitchCategory[is.na(new_data$PitchCategory)] <- "Undefined"
      new_data$PitchCategory <- factor(new_data$PitchCategory,
        levels = c("Fastball", "Breaking Ball", "Offspeed", "Undefined"))
      new_data$Season <- "Summer 2026"
      app_data(new_data)
      if (exists("DATA_CLEAN_SUMMARY")) clean_summary_rv(DATA_CLEAN_SUMMARY)

      if (is.null(input$main_tabs) || input$main_tabs == "Pitching") {
        updatePickerInput(session, "player",
          choices  = c("All Players",
            sort(unique(new_data$Pitcher[new_data$PitcherTeam %in% SEAGULLS_TEAM]))),
          selected = "All Players"
        )
      } else {
        updatePickerInput(session, "player",
          choices  = c("All Players",
            sort(unique(new_data$Batter[new_data$BatterTeam %in% SEAGULLS_TEAM]))),
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

  player_selected_games <- reactive({
    date_col  <- if (user_player_type() == "pitcher") "Pitcher" else "Batter"
    all_dates <- app_data() %>%
      filter(.data[[date_col]] == user_player_name()) %>%
      pull(Date) %>% unique() %>% sort(decreasing = TRUE)

    switch(input$player_game_window %||% "This Game",
      "Season"    = all_dates,
      selected_game()   # default: the currently navigated single game
    )
  })

  player_fdata <- reactive({
    req(!is.null(input$player_pitch_cats), length(input$player_pitch_cats) > 0)
    date_col <- if (user_player_type() == "pitcher") "Pitcher" else "Batter"
    count_filter <- switch(input$player_count,
      "All"             = NULL,
      "Pitcher's Count" = PITCHER_COUNTS,
      "Hitter's Count"  = HITTER_COUNTS,
      "2K"              = TWO_K_COUNTS
    )

    d <- app_data() %>%
      filter(
        .data[[date_col]] == user_player_name(),
        Date %in% player_selected_games(),
        PitchCategory %in% input$player_pitch_cats,
        PitchCategory != "Undefined"
      )

    if (!is.null(count_filter))
      d <- d %>% filter(Count %in% count_filter)

    if (user_player_type() %in% c("pitcher", "two-way"))
      d <- d %>% filter(Inning >= input$player_innings[1],
                        Inning <= input$player_innings[2])

    d
  })

  # ── Player: game navigator UI ────────────────────────────────────────────
  output$player_game_nav <- renderUI({
    if (is.null(input$player_game_window) || input$player_game_window != "This Game")
      return(NULL)
    g <- player_games()
    if (length(g) == 0)
      return(div("No games recorded yet.",
                 style = "color:#888; font-size:12px; padding:6px 0;"))
    idx      <- game_index()
    date_str <- tryCatch(format(selected_game(), "%b %d, %Y"), error = function(e) "—")
    at_start <- idx >= length(g)
    at_end   <- idx <= 1L
    div(
      style = "margin-top:6px; padding:8px 10px; background:#f8f9fc;
               border-radius:8px; border:1px solid #e2e8f0;",
      tags$p(date_str,
             style = "color:#1a202c; font-size:13px; font-weight:600;
                      text-align:center; margin:0 0 2px;"),
      tags$p(paste0("Game ", length(g) - idx + 1L, " of ", length(g)),
             style = "color:#64748b; font-size:11px; text-align:center; margin:0 0 8px;"),
      div(style = "display:flex; gap:6px;",
          actionButton("prev_game", "← Prev",
                       style = paste0("flex:1; font-size:11px; padding:3px 0;",
                                      if (at_start) " opacity:0.4;" else "")),
          actionButton("next_game", "Next →",
                       style = paste0("flex:1; font-size:11px; padding:3px 0;",
                                      if (at_end) " opacity:0.4;" else "")))
    )
  })

  # ── Player spray click filter ─────────────────────────────────────────────
  player_spray_click_filter <- reactiveVal(NULL)

  observeEvent(list(input$player_game_window, game_index()), {
    player_spray_click_filter(NULL)
  })

  observeEvent(event_data("plotly_click", source = "player_spray_chart"), {
    click <- event_data("plotly_click", source = "player_spray_chart")
    if (!is.null(click$customdata)) {
      clicked <- click$customdata[[1]]
      if (identical(player_spray_click_filter(), clicked)) {
        player_spray_click_filter(NULL)
      } else {
        player_spray_click_filter(clicked)
      }
    }
  })

  observeEvent(event_data("plotly_doubleclick", source = "player_spray_chart"), {
    player_spray_click_filter(NULL)
  })

  player_fdata_hitting <- reactive({
    d <- player_fdata()
    if (!is.null(player_spray_click_filter()))
      d <- d %>% filter(PlayResult == player_spray_click_filter())
    d
  })

  # Trend baseline: 5 most recent games not in the current window
  player_recent_fdata <- reactive({
    req(user_role() == "player")
    date_col   <- if (user_player_type() == "pitcher") "Pitcher" else "Batter"
    sel_dates  <- player_selected_games()
    all_dates  <- app_data() %>%
      filter(.data[[date_col]] == user_player_name()) %>%
      pull(Date) %>% unique() %>% sort(decreasing = TRUE)
    past_dates <- head(all_dates[!all_dates %in% sel_dates], 5L)
    if (length(past_dates) == 0L)
      return(app_data() %>% filter(FALSE))
    app_data() %>%
      filter(.data[[date_col]] == user_player_name(), Date %in% past_dates)
  })

  # ── Player: main content (sidebar handles identity + game nav) ───────────
  output$player_ui <- renderUI({
    req(user_role() == "player")

    pname  <- user_player_name()
    ptype  <- user_player_type()

    roster_row <- roster_positions %>% filter(player_name == pname)
    jersey     <- if (nrow(roster_row) > 0) roster_row$jersey[1]   else ""
    position   <- if (nrow(roster_row) > 0) roster_row$position[1] else ""

    photo_tag  <- player_photo_tag(pname, size = "72px")

    date_col   <- if (ptype == "pitcher") "Pitcher" else "Batter"
    game_dates <- app_data() %>%
      filter(.data[[date_col]] == pname) %>%
      pull(Date) %>% unique() %>% sort(decreasing = TRUE)

    fluidPage(
      fluidRow(
        column(3, class = "player-sidebar-col",
          div(style = "background:#0a1628; min-height:100vh; padding:0;
                       display:flex; flex-direction:column;",

            # Header card — logo / team name / user row
            div(style = "margin:10px; border-radius:12px; overflow:hidden;
                         background:#015294; border:1px solid rgba(255,255,255,0.12);
                         padding:20px 16px 16px 16px;",

              # Row 1 — Team logo
              div(style = "text-align:center; margin-bottom:8px;",
                tags$img(src = "seagulls_logo.png", height = "56px",
                         style = "display:inline-block;")
              ),

              # Row 2 — Team name
              div("SF Seagulls",
                  style = "text-align:center; color:#ffffff; font-size:14px;
                           font-weight:700; margin-bottom:14px; letter-spacing:0.3px;"),

              # Row 3 — Name + role (left) / Log Out (right)
              div(style = "display:flex; align-items:center;
                           justify-content:space-between; margin-bottom:14px;",
                div(
                  div(pname,
                      style = "color:#ffffff; font-size:13px; font-weight:600;
                               line-height:1.3;"),
                  div(paste0("(", switch(ptype,
                               "pitcher"  = "Pitcher",
                               "hitter"   = "Hitter",
                               "two-way"  = "Two-Way",
                               ptype), ")"),
                      style = "color:#a8c8e8; font-size:11px;")
                ),
                actionButton("logout", "Log Out",
                             style = "font-size:11px; padding:3px 10px;
                                      background:transparent; color:#ffffff;
                                      border:1px solid rgba(255,255,255,0.5);
                                      border-radius:4px; flex-shrink:0;")
              ),

              div(style = "border-top:1px solid rgba(255,255,255,0.15); margin:0 -16px;")
            ),

            # White filter card
            div(style = "background:#ffffff; margin:0 10px 10px 10px;
                         padding:14px; border-radius:10px; flex:1; overflow-y:auto;",

              tags$label("Season", style = "font-weight:600; font-size:12px;"),
              selectInput("player_season", label = NULL,
                choices = c("Summer 2026"), selected = "Summer 2026", width = "100%"),

              tags$label("Games", style = "font-weight:600; font-size:12px; margin-top:8px; display:block;"),
              radioGroupButtons("player_game_window", label = NULL,
                choices  = c("This Game", "Season"),
                selected = "This Game", size = "sm", justified = TRUE),
              uiOutput("player_game_nav"),

              # Analyst-only filters — hidden on phones (see custom.css) so the
              # player just navigates games; defaults show the full game.
              div(class = "player-secondary-filters",
                hr(),

                tags$label("Pitch Category", style = "font-weight:600; font-size:12px;"),
                pickerInput("player_pitch_cats", label = NULL,
                  choices  = c("Fast" = "Fastball", "Breaking" = "Breaking Ball", "Off" = "Offspeed"),
                  selected = c("Fastball", "Breaking Ball", "Offspeed"),
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE)),

                hr(),

                tags$label("Count", style = "font-weight:600; font-size:12px;"),
                selectInput("player_count", label = NULL,
                  choices  = c("All", "Pitcher's Count", "Hitter's Count", "2K"),
                  selected = "All", width = "100%"),

                if (ptype %in% c("pitcher", "two-way")) tagList(
                  hr(),
                  tags$label("Innings", style = "font-weight:600; font-size:12px;"),
                  sliderInput("player_innings", label = NULL,
                    min = 1, max = 9, value = c(1, 9), step = 1, width = "100%")
                )
              )
            )
          )
        ),

        column(9, class = "player-content-col", uiOutput("player_content"))
      )
    )
  })

  # ── Player: content router ───────────────────────────────────────────────
  output$player_content <- renderUI({
    req(user_role() == "player")
    ptype <- user_player_type()
    if (ptype %in% c("pitcher", "two-way")) {
      uiOutput("player_pitcher_section")
    } else {
      uiOutput("player_hitter_section")
    }
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

    tabsetPanel(
      id = "player_pitcher_tabs",
      tabPanel("Overview",
        tags$h6("Pitching", style = "color:#0a1628; font-weight:600; margin:12px 0 8px;"),
        layout_columns(
          col_widths = breakpoints(xs = 6, sm = 6, md = 3),
          stat_tile(tagList("Strike%", metric_badge(spct, 0.65, 0.53)),
                    fmt(spct), tile_class(spct, 0.65, 0.53),
                    trend        = mk_trend(spct, spct_base),
                    tooltip_text = "Strikes, swings, and balls in play as a share of all pitches."),
          stat_tile(tagList("Whiff%", metric_badge(wpct, 0.29, 0.13)),
                    fmt(wpct), tile_class(wpct, 0.29, 0.13),
                    trend        = mk_trend(wpct, wpct_base),
                    tooltip_text = "Share of swings that completely missed. Top arms in this league are around 29%+."),
          stat_tile(tagList("CSW%", metric_badge(cswp, 0.31, 0.21)),
                    fmt(cswp), tile_class(cswp, 0.31, 0.21),
                    trend        = mk_trend(cswp, cswp_base),
                    tooltip_text = "Called Strikes + Whiffs per pitch — the best single-pitch quality metric."),
          stat_tile(tagList("Chase%", metric_badge(chsp, 0.30, 0.20)),
                    fmt(chsp), tile_class(chsp, 0.30, 0.20),
                    trend        = mk_trend(chsp, chsp_base),
                    tooltip_text = "How often hitters chased pitches outside the zone.")
        ),
        tags$h5("Pitch Arsenal Summary",
                style = "font-weight:700; color:#0a1628; font-size:15px; margin:16px 0 8px 4px; letter-spacing:0.2px;"),
        DT::DTOutput("player_pitch_summary_tbl"),
        layout_columns(
          col_widths = breakpoints(sm = 12, md = 6),
          plotlyOutput("player_zone13",   height = "360px"),
          plotlyOutput("player_movement", height = "360px")
        ),
        tags$h5("Pitch Log",
                style = "font-weight:700; color:#0a1628; font-size:15px; margin:16px 0 8px 4px; letter-spacing:0.2px;"),
        DTOutput("player_pitch_log")
      ),
      tabPanel("Game Log",
        tags$h5("Game-by-Game Stats",
                style = "font-weight:700; color:#0a1628; font-size:15px; margin:16px 0 8px 4px;"),
        DTOutput("player_pitcher_game_log")
      )
    )
  })

  output$player_zone13 <- renderPlotly({
    req(nrow(player_fdata()) > 0)

    # --- Classify pitches into zones 1-9 (inner) and 11-14 (outer) ---
    zw <- (SZ_RIGHT - SZ_LEFT) / 3
    zh <- (SZ_TOP   - SZ_BOT)  / 3

    d_raw <- player_fdata() %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        in_col = cut(PlateLocSide,
          breaks = c(SZ_LEFT, SZ_LEFT+zw, SZ_LEFT+2*zw, SZ_RIGHT),
          labels = c("L","M","R"), include.lowest = TRUE),
        in_row = cut(PlateLocHeight,
          breaks = c(SZ_BOT, SZ_BOT+zh, SZ_BOT+2*zh, SZ_TOP),
          labels = c("Low","Mid","High"), include.lowest = TRUE),
        in_zone = !is.na(in_col) & !is.na(in_row),
        near_zone = PlateLocSide  >= SZ_LEFT - zw &
                    PlateLocSide  <= SZ_RIGHT + zw &
                    PlateLocHeight >= SZ_BOT - zh &
                    PlateLocHeight <= SZ_TOP + zh,
        zone = case_when(
          in_zone & in_col=="L" & in_row=="High" ~ "1",
          in_zone & in_col=="M" & in_row=="High" ~ "2",
          in_zone & in_col=="R" & in_row=="High" ~ "3",
          in_zone & in_col=="L" & in_row=="Mid"  ~ "4",       in_zone & in_col=="M" & in_row=="Mid"  ~ "5",
          in_zone & in_col=="R" & in_row=="Mid"  ~ "6",
          in_zone & in_col=="L" & in_row=="Low"  ~ "7",
          in_zone & in_col=="M" & in_row=="Low"  ~ "8",
          in_zone & in_col=="R" & in_row=="Low"  ~ "9",
          !in_zone & near_zone & PlateLocSide <  0 & PlateLocHeight >= (SZ_BOT+SZ_TOP)/2 ~ "11",
          !in_zone & near_zone & PlateLocSide >= 0 & PlateLocHeight >= (SZ_BOT+SZ_TOP)/2 ~ "12",
          !in_zone & near_zone & PlateLocSide <  0 & PlateLocHeight <  (SZ_BOT+SZ_TOP)/2 ~ "13",
          !in_zone & near_zone & PlateLocSide >= 0 & PlateLocHeight <  (SZ_BOT+SZ_TOP)/2 ~ "14",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(zone)) %>%
      count(zone) %>%
      mutate(pct = n / sum(n))

    # Fill in any missing zones with 0
    all_zones <- tibble(zone = as.character(c(1:9, 11:14)))
    d_raw <- left_join(all_zones, d_raw, by = "zone") %>%
      mutate(pct = replace_na(pct, 0), n = replace_na(n, 0))

    pct_lookup <- setNames(d_raw$pct, d_raw$zone)

    # --- Build rectangle data frame for drawing ---
    # Coordinate system: inner grid spans x[-1.5,1.5], y[-1.5,1.5] # Outer margin = 1 unit on every side → outer boundary x[-2.5,2.5], y[-2.5,2.5]

    inner_cells <- tribble(
      ~zone, ~xmin, ~xmax, ~ymin, ~ymax, ~lx,  ~ly,
      "1",  -1.5,  -0.5,   0.5,   1.5,  -1.0,  1.0,
      "2",  -0.5,   0.5,   0.5,   1.5,   0.0,  1.0,
      "3",   0.5,   1.5,   0.5,   1.5,   1.0,  1.0,
      "4",  -1.5,  -0.5,  -0.5,   0.5,  -1.0,  0.0,
      "5",  -0.5,   0.5,  -0.5,   0.5,   0.0,  0.0,
      "6",   0.5,   1.5,  -0.5,   0.5,   1.0,  0.0,
      "7",  -1.5,  -0.5,  -1.5,  -0.5,  -1.0, -1.0,
      "8",  -0.5,   0.5,  -1.5,  -0.5,   0.0, -1.0,
      "9",   0.5,   1.5,  -1.5,  -0.5,   1.0, -1.0
    )

    # Outer zones drawn as quadrants first (inner grid draws on top)
    outer_cells <- tribble(
      ~zone, ~xmin, ~xmax, ~ymin, ~ymax, ~lx,  ~ly,
      "11", -2.5,   0.0,   0.0,   2.5,  -2.0,  2.0,
      "12",  0.0,   2.5,   0.0,   2.5,   2.0,  2.0,
      "13", -2.5,   0.0,  -2.5,   0.0,  -2.0, -2.0,
      "14",  0.0,   2.5,  -2.5,   0.0,   2.0, -2.0
    )

    add_pct <- function(df) {
      df %>% mutate(
        pct_val  = pct_lookup[zone],
        pct_lab  = if_else(is.na(pct_val) | pct_val == 0, "—",
                           paste0(round(pct_val * 100), "%")),
        txt_col  = ifelse(!is.na(pct_val) & pct_val >= 0.15, "white", "#1a1a2e")
      )
    }

    outer_cells <- add_pct(outer_cells)
    inner_cells <- add_pct(inner_cells)

    fill_scale <- colorRampPalette(c("#ffffff", "#ffffff", "#f5c0b0", "#C0392B"))(100)
    fill_fn    <- function(v) {
      idx <- pmax(1, pmin(100, round((pmax(v - 0.05, 0) / 0.15) * 80) + 1))
      fill_scale[idx]
    }

    p <- ggplot() +
      # Outer zones first (drawn behind inner grid)
      geom_rect(data = outer_cells,
                aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill = fill_fn(outer_cells$pct_val),
                color = "black", linewidth = 1.2) +
      geom_text(data = outer_cells,
                aes(x=lx, y=ly, label=pct_lab, color=txt_col),
                size = 4, fontface = "bold") +
      # Inner grid on top
      geom_rect(data = inner_cells,
                aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill = fill_fn(inner_cells$pct_val),
                color = "black", linewidth = 1.2) +
      geom_text(data = inner_cells,
                aes(x=lx, y=ly, label=pct_lab, color=txt_col),
                size = 4, fontface = "bold") +
      scale_color_identity() +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5), expand = FALSE) +
      labs(title    = "Zone Profile (Catcher's View)",
           subtitle = "Catcher's-eye view — looking out toward the pitcher",
           x = NULL, y = NULL,
           caption  = "Zone from Catcher's Perspective") +
      theme_seagulls() +
      theme(axis.text  = element_blank(), axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 13,
                                      color = "#0a1628", margin = margin(b = 6)))

    ggplotly(p, tooltip = character(0)) %>%
      layout(
        yaxis         = list(scaleanchor = "x", scaleratio = 1,
                             range = c(-2.6, 2.6), visible = FALSE),
        xaxis         = list(range = c(-2.6, 2.6), visible = FALSE),
        margin        = list(t = 40, b = 40, l = 20, r = 20),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      ) %>%
      config(displayModeBar = FALSE) %>%
      style(hoverinfo = "none")
  })

  output$player_pitch_summary_tbl <- DT::renderDT({
    req(user_role() == "player")
    req(user_player_type() %in% c("pitcher", "two-way"))
    req(nrow(player_fdata()) > 0)

    total <- nrow(player_fdata())
    d <- player_fdata() %>%
      group_by(Pitch = PitchCategory) %>%
      summarise(
        `Usage%`   = scales::percent(n() / total, accuracy = 1),
        `Avg Velo` = round(mean(RelSpeed,          na.rm = TRUE), 1),
        `Max Velo` = round(max(RelSpeed,           na.rm = TRUE), 1),
        `Avg Spin` = round(mean(SpinRate,          na.rm = TRUE), 0),
        `Avg IVB`  = round(mean(InducedVertBreak,  na.rm = TRUE), 1),
        `Avg HB`   = round(mean(HorzBreak,         na.rm = TRUE), 1),
        `Whiff%`   = whiff_pct(PitchCall),
        .groups    = "drop"
      ) %>%
      arrange(desc(as.numeric(sub("%", "", `Usage%`))))

    d <- d %>%
      mutate(Pitch = paste0(
        '<span style="display:inline-block;width:10px;height:10px;border-radius:50%;',
        'vertical-align:middle;margin-right:7px;background:',
        vapply(as.character(Pitch), function(x) {
          col <- PITCH_CATEGORY_COLORS[[x]]
          if (is.null(col) || is.na(col)) "#AAAAAA" else col
        }, character(1)),
        ';"></span>', Pitch
      ))

    DT::datatable(d, rownames = FALSE, escape = FALSE,
      options = list(pageLength = 15, dom = "t", ordering = TRUE),
      class   = "compact stripe"
    ) %>%
      DT::formatRound(c("Avg Velo", "Max Velo", "Avg IVB", "Avg HB"), digits = 1) %>%
      DT::formatRound("Avg Spin", digits = 0, mark = ",") %>%
      DT::formatPercentage("Whiff%", digits = 1) %>%
      DT::formatStyle(columns = names(d), textAlign = "right")
  })

  output$player_movement <- renderPlotly({
    req(nrow(player_fdata()) > 0)
    d <- player_fdata() %>%
      group_by(PitchCategory) %>%
      summarise(
        HorzBreak        = round(mean(HorzBreak,        na.rm = TRUE), 2),
        InducedVertBreak = round(mean(InducedVertBreak, na.rm = TRUE), 2),
        n                = n(),
        .groups          = "drop"
      )
    rings <- bind_rows(lapply(c(6, 12, 18), ring_df))
    p <- ggplot(d, aes(
        x = HorzBreak, y = InducedVertBreak,
        color = PitchCategory, size = n,
        label = PitchCategory
      )) +
      geom_path(data = rings, aes(x = x, y = y, group = r),
                color = "#D1D5DB", linetype = "dashed", linewidth = 0.4,
                inherit.aes = FALSE) +
      geom_hline(yintercept = 0, color = "#CBD5E1", linewidth = 0.5) +
      geom_vline(xintercept = 0, color = "#CBD5E1", linewidth = 0.5) +
      geom_point(alpha = 0.85) +
      geom_text(aes(label = PitchCategory),
                size = 3.2, hjust = -0.2, vjust = 0.5,
                show.legend = FALSE) +
      scale_color_manual(values = PITCH_CATEGORY_COLORS) +
      scale_size_continuous(range = c(4, 12)) +
      coord_fixed() +
      labs(
        title    = "Movement Profile (Pitcher's View)",
        subtitle = "Pitcher's-eye view — like the TV camera behind the mound",
        x = "Horizontal Break (in)", y = "Induced Vert Break (in)",
        color = NULL, size = "Pitches"
      ) +
      theme_seagulls() +
      theme(legend.position = if (n_distinct(d$PitchCategory) <= 1) "none" else "right")
    plotly_clean(ggplotly(p, tooltip = c("label", "x", "y", "size")))
  })

  # ── Player: pitcher pitch log ─────────────────────────────────────────────
  output$player_pitch_log <- DT::renderDT({
    req(user_role() == "player")
    d <- player_fdata()
    req(nrow(d) > 0)
    d %>%
      arrange(Inning, PAofInning, PitchofPA) %>%
      transmute(
        Inn    = Inning,
        Batter,
        Count,
        Type   = TaggedPitchType,
        `Velo` = round(RelSpeed, 1),
        Result = format_pitch_result(PitchCall, PlayResult)
      ) %>%
      DT::datatable(
        rownames = FALSE,
        options  = list(pageLength = 15, dom = "tip", scrollX = TRUE,
                        columnDefs = list(list(className = "dt-center", targets = "_all"))),
        class    = "table-sm table-striped"
      )
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

    tabsetPanel(
      id = "player_hitter_tabs",
      tabPanel("Overview",
        tags$h6("Hitting", style = "color:#0a1628; font-weight:600; margin:12px 0 8px;"),
        layout_columns(
          col_widths = breakpoints(xs = 6, sm = 6, md = 3),
          stat_tile(tagList("Avg EV", metric_badge(avg_ev, 84, 73)),
                    fmt_ev(avg_ev), tile_class(avg_ev, 84, 73),
                    trend        = mk_trend_ev(avg_ev, avg_ev_base),
                    tooltip_text = "Average exit velocity on contact. League average ~79 mph; 84+ is above average here."),
          stat_tile(tagList("Hard Hit%", metric_badge(hh, 0.50, 0.30)),
                    fmt_pct(hh), tile_class(hh, 0.50, 0.30),
                    trend        = mk_trend(hh, hh_base),
                    tooltip_text = "Share of batted balls at 85+ mph (the hard-hit bar for this league). League median ~37%; 50%+ is strong."),
          stat_tile(tagList("Zone Swing%", metric_badge(swing_zone, 0.78, 0.60)),
                    fmt_pct(swing_zone), tile_class(swing_zone, 0.78, 0.60),
                    trend        = mk_trend(swing_zone, swing_zone_base),
                    tooltip_text = "How often you swung at pitches inside the strike zone — attacking hittable pitches."),
          stat_tile(tagList("Chase%", metric_badge(chase, 0.22, 0.32, hi_good = FALSE)),
                    fmt_pct(chase), tile_class(chase, 0.22, 0.32, hi_good = FALSE),
                    trend        = mk_trend(chase, chase_base),
                    tooltip_text = "Chase rate — how often you swung at pitches outside the zone. MLB avg ~30%; lower is better.")
        ),
        layout_columns(
          col_widths = breakpoints(sm = 12, md = 6),
          plotlyOutput("player_spray",       height = "360px"),
          plotlyOutput("player_swing_zones", height = "360px")
        ),
        tags$h5("Pitch Log",
                style = "font-weight:700; color:#0a1628; font-size:15px; margin:16px 0 8px 4px; letter-spacing:0.2px;"),
        DTOutput("player_hit_log")
      ),
      tabPanel("Game Log",
        tags$h5("Game-by-Game Stats",
                style = "font-weight:700; color:#0a1628; font-size:15px; margin:16px 0 8px 4px;"),
        DTOutput("player_hitter_game_log")
      )
    )
  })

  output$player_spray <- renderPlotly({
    req(nrow(player_fdata()) > 0)

    d <- player_fdata() %>%
      filter(PlayResult %in% c("Single","Double","Triple","HomeRun","Out"),
             !is.na(Direction), !is.na(Distance)) %>%
      mutate(
        spray_x = Distance * sin(Direction * pi / 180),
        spray_y = Distance * cos(Direction * pi / 180)
      )
    req(nrow(d) > 0)

    result_colors <- c(Single = "#2DC653", Double = "#F5C518",
                       Triple = "#FF8C00", HomeRun = "#E63946", Out = "#AAAAAA")
    active <- player_spray_click_filter()

    d <- d %>% mutate(
      alpha_val = if (is.null(active)) 0.75
                  else ifelse(PlayResult == active, 0.95, 0.12),
      size_val  = if (is.null(active)) 6
                  else ifelse(PlayResult == active, 8, 4)
    )

    outline <- field_outline_df()

    title_txt <- if (!is.null(active))
      paste0("Batted Ball Chart — ", active,
             "s only  <i>(click again or double-click to clear)</i>")
    else
      "Batted Ball Chart  <i>(click a dot to filter by result type)</i>"

    p <- plot_ly(source = "player_spray_chart") %>%
      add_trace(
        data = outline, type = "scatter", mode = "lines",
        x = ~x, y = ~y,
        line = list(color = "gray70", width = 1.5),
        hoverinfo = "none", showlegend = FALSE
      )

    for (result in c("HomeRun","Triple","Double","Single","Out")) {
      sub <- d %>% filter(PlayResult == result)
      if (nrow(sub) == 0) next
      p <- p %>% add_trace(
        data       = sub,
        type       = "scatter", mode = "markers",
        x          = ~spray_x, y = ~spray_y,
        name       = result,
        customdata = ~PlayResult,
        marker     = list(
          color   = result_colors[result],
          size    = sub$size_val,
          opacity = sub$alpha_val,
          line    = list(width = 0)
        ),
        text = ~paste0(PlayResult,
                       "<br>EV: ", ifelse(is.na(ExitSpeed), "—",
                                          paste0(round(ExitSpeed, 1), " mph")),
                       "<br>Dist: ", ifelse(is.na(Distance), "—",
                                            paste0(round(Distance), " ft"))),
        hoverinfo = "text"
      )
    }

    p %>% layout(
      title         = list(text = title_txt, font = list(size = 13), x = 0),
      xaxis         = list(visible = FALSE, range = c(-350, 350)),
      yaxis         = list(visible = FALSE, range = c(-30, 430),
                           scaleanchor = "x", scaleratio = 1),
      showlegend    = TRUE,
      legend        = list(orientation = "v"),
      paper_bgcolor = "white", plot_bgcolor = "white"
    ) %>%
      config(displayModeBar = FALSE)
  })

  output$player_swing_zones <- renderPlotly({
    req(nrow(player_fdata()) > 0)

    zw <- (SZ_RIGHT - SZ_LEFT) / 3
    zh <- (SZ_TOP   - SZ_BOT)  / 3

    d_raw <- player_fdata() %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        in_col = cut(PlateLocSide,
          breaks = c(SZ_LEFT, SZ_LEFT+zw, SZ_LEFT+2*zw, SZ_RIGHT),
          labels = c("L","M","R"), include.lowest = TRUE),
        in_row = cut(PlateLocHeight,
          breaks = c(SZ_BOT, SZ_BOT+zh, SZ_BOT+2*zh, SZ_TOP),
          labels = c("Low","Mid","High"), include.lowest = TRUE),
        in_zone   = !is.na(in_col) & !is.na(in_row),
        near_zone = PlateLocSide  >= SZ_LEFT - zw &
                    PlateLocSide  <= SZ_RIGHT + zw &
                    PlateLocHeight >= SZ_BOT - zh &
                    PlateLocHeight <= SZ_TOP + zh,
        swing = PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable",
                                  "FoulBallFieldable","InPlay"),
        zone = case_when(
          in_zone & in_col=="L" & in_row=="High" ~ "1",
          in_zone & in_col=="M" & in_row=="High" ~ "2",
          in_zone & in_col=="R" & in_row=="High" ~ "3",
          in_zone & in_col=="L" & in_row=="Mid"  ~ "4",
          in_zone & in_col=="M" & in_row=="Mid"  ~ "5",
          in_zone & in_col=="R" & in_row=="Mid"  ~ "6",
          in_zone & in_col=="L" & in_row=="Low"  ~ "7",
          in_zone & in_col=="M" & in_row=="Low"  ~ "8",
          in_zone & in_col=="R" & in_row=="Low"  ~ "9",
          !in_zone & near_zone & PlateLocSide <  0 & PlateLocHeight >= (SZ_BOT+SZ_TOP)/2 ~ "11",
          !in_zone & near_zone & PlateLocSide >= 0 & PlateLocHeight >= (SZ_BOT+SZ_TOP)/2 ~ "12",
          !in_zone & near_zone & PlateLocSide <  0 & PlateLocHeight <  (SZ_BOT+SZ_TOP)/2 ~ "13",
          !in_zone & near_zone & PlateLocSide >= 0 & PlateLocHeight <  (SZ_BOT+SZ_TOP)/2 ~ "14",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(zone)) %>%
      group_by(zone) %>%
      summarise(pct_val = mean(swing), n = n(), .groups = "drop")

    all_zones <- tibble(zone = as.character(c(1:9, 11:14)))
    d_raw <- left_join(all_zones, d_raw, by = "zone") %>%
      mutate(pct_val = replace_na(pct_val, 0), n = replace_na(n, 0L))

    pct_lookup <- setNames(d_raw$pct_val, d_raw$zone)

    outer_cells <- tribble(
      ~zone, ~xmin, ~xmax, ~ymin, ~ymax, ~lx,  ~ly,
      "11", -2.5,   0.0,   0.0,   2.5,  -2.0,  2.0,
      "12",  0.0,   2.5,   0.0,   2.5,   2.0,  2.0,
      "13", -2.5,   0.0,  -2.5,   0.0,  -2.0, -2.0,
      "14",  0.0,   2.5,  -2.5,   0.0,   2.0, -2.0
    )

    inner_cells <- tribble(
      ~zone, ~xmin, ~xmax, ~ymin, ~ymax, ~lx,  ~ly,
      "1",  -1.5,  -0.5,   0.5,   1.5,  -1.0,  1.0,
      "2",  -0.5,   0.5,   0.5,   1.5,   0.0,  1.0,
      "3",   0.5,   1.5,   0.5,   1.5,   1.0,  1.0,
      "4",  -1.5,  -0.5,  -0.5,   0.5,  -1.0,  0.0,
      "5",  -0.5,   0.5,  -0.5,   0.5,   0.0,  0.0,
      "6",   0.5,   1.5,  -0.5,   0.5,   1.0,  0.0,
      "7",  -1.5,  -0.5,  -1.5,  -0.5,  -1.0, -1.0,
      "8",  -0.5,   0.5,  -1.5,  -0.5,   0.0, -1.0,
      "9",   0.5,   1.5,  -1.5,  -0.5,   1.0, -1.0
    )

    add_pct <- function(df) {
      df %>% mutate(
        pct_val = pct_lookup[zone],
        pct_lab = if_else(is.na(pct_val) | pct_val == 0, "—",
                          paste0(round(pct_val * 100), "%")),
        txt_col = ifelse(!is.na(pct_val) & pct_val >= 0.55, "white", "#1a1a2e")
      )
    }

    outer_cells <- add_pct(outer_cells)
    inner_cells <- add_pct(inner_cells)

    fill_scale <- colorRampPalette(c("#ffffff", "#f5c0b0", "#C0392B"))(100)
    fill_fn    <- function(v) {
      idx <- pmax(1, pmin(100, round((pmax(v - 0.20, 0) / 0.60) * 99) + 1))
      fill_scale[idx]
    }

    p <- ggplot() +
      geom_rect(data = outer_cells,
                aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill = fill_fn(outer_cells$pct_val),
                color = "black", linewidth = 1.2) +
      geom_text(data = outer_cells,
                aes(x=lx, y=ly, label=pct_lab, color=txt_col),
                size = 4, fontface = "bold") +
      geom_rect(data = inner_cells,
                aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill = fill_fn(inner_cells$pct_val),
                color = "black", linewidth = 1.2) +
      geom_text(data = inner_cells,
                aes(x=lx, y=ly, label=pct_lab, color=txt_col),
                size = 4, fontface = "bold") +
      scale_color_identity() +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5), expand = FALSE) +
      labs(title    = "Swing Rates by Zone (Catcher's View)",
           subtitle = "Catcher's-eye view — looking out toward the pitcher",
           x = NULL, y = NULL,
           caption  = "Zone from Catcher's Perspective") +
      theme_seagulls() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid = element_blank())

    ggplotly(p, tooltip = character(0)) %>%
      layout(
        yaxis         = list(scaleanchor = "x", scaleratio = 1,
                             range = c(-2.6, 2.6), visible = FALSE),
        xaxis         = list(range = c(-2.6, 2.6), visible = FALSE),
        margin        = list(t = 40, b = 40, l = 20, r = 20),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      ) %>%
      config(displayModeBar = FALSE) %>%
      style(hoverinfo = "none")
  })

  # ── Player: pitcher game log ──────────────────────────────────────────────
  output$player_pitcher_game_log <- DT::renderDT({
    req(user_role() == "player")
    req(user_player_type() %in% c("pitcher", "two-way"))
    d <- player_fdata_base() %>%
      group_by(Date) %>%
      summarise(
        Pitches   = n(),
        BF        = n_distinct(paste(Inning, PAofInning)),
        K         = sum(KorBB == "Strikeout", na.rm = TRUE),
        BB        = sum(KorBB == "Walk",      na.rm = TRUE),
        `Strike%` = strike_pct(PitchCall),
        `Whiff%`  = whiff_pct(PitchCall),
        `CSW%`    = csw_pct(PitchCall),
        `Avg Velo` = round(mean(RelSpeed, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(Date)) %>%
      mutate(Date = format(Date, "%b %d, %Y"))
    req(nrow(d) > 0)
    DT::datatable(d, rownames = FALSE,
      options = list(pageLength = 20, dom = "t", ordering = FALSE),
      class = "compact stripe"
    ) %>%
      DT::formatPercentage(c("Strike%", "Whiff%", "CSW%"), digits = 1) %>%
      DT::formatRound("Avg Velo", digits = 1) %>%
      DT::formatStyle("Date", textAlign = "left") %>%
      DT::formatStyle(c("Pitches","BF","K","BB","Strike%","Whiff%","CSW%","Avg Velo"),
                      textAlign = "center")
  })

  # ── Player: hitter game log ───────────────────────────────────────────────
  output$player_hitter_game_log <- DT::renderDT({
    req(user_role() == "player")
    req(user_player_type() == "hitter")
    d <- player_fdata_base() %>%
      group_by(Date) %>%
      summarise(
        PA          = n_distinct(paste(Inning, PAofInning)),
        H           = sum(PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm = TRUE),
        HR          = sum(PlayResult == "HomeRun", na.rm = TRUE),
        BB          = sum(KorBB == "Walk",      na.rm = TRUE),
        K           = sum(KorBB == "Strikeout", na.rm = TRUE),
        `Avg EV`    = round(mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE), 1),
        `Hard Hit%` = hard_hit_pct(ExitSpeed[PitchCall == "InPlay"]),
        .groups = "drop"
      ) %>%
      arrange(desc(Date)) %>%
      mutate(Date = format(Date, "%b %d, %Y"))
    req(nrow(d) > 0)
    DT::datatable(d, rownames = FALSE,
      options = list(pageLength = 20, dom = "t", ordering = FALSE),
      class = "compact stripe"
    ) %>%
      DT::formatPercentage("Hard Hit%", digits = 1) %>%
      DT::formatRound("Avg EV", digits = 1) %>%
      DT::formatStyle("Date", textAlign = "left") %>%
      DT::formatStyle(c("PA","H","HR","BB","K","Avg EV","Hard Hit%"),
                      textAlign = "center")
  })

  # ── Player: hitter pitch log ───────────────────────────────────────────────
  output$player_hit_log <- DT::renderDT({
    req(user_role() == "player")
    d <- player_fdata()
    req(nrow(d) > 0)
    d %>%
      arrange(Inning, PAofInning, PitchofPA) %>%
      transmute(
        Inn     = Inning,
        Pitcher,
        Count,
        Type    = TaggedPitchType,
        Velo    = round(RelSpeed, 1),
        `EV`    = ifelse(PitchCall == "InPlay" & !is.na(ExitSpeed),
                         paste0(round(ExitSpeed, 1), " mph"), "—"),
        Result  = format_pitch_result(PitchCall, PlayResult)
      ) %>%
      DT::datatable(
        rownames = FALSE,
        options  = list(pageLength = 15, dom = "tip", scrollX = TRUE,
                        columnDefs = list(list(className = "dt-center", targets = "_all"))),
        class    = "table-sm table-striped"
      )
  })

}
