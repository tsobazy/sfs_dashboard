# SF Seagulls Dashboard — Full Redesign Spec
**Date:** 2026-06-17
**Scope:** Visual language, navigation, coach view, player view, pitch grouping

---

## Goal
Replace the current amateur-looking, over-complicated dashboard with a clean, professional analytics tool. Coaches get a game-by-game review flow with the right charts in the right order. Players get a personal-only view with no teammate comparisons.

## Architecture
Same R Shiny stack (`global.R` / `server.R` / `ui.R`). No new files except possibly `theme.R` to centralise the new color/font constants. All changes are in-place replacements of existing outputs and layout helpers.

---

## Global Constraints
- Never show player-vs-player comparison data in the player portal — no team avg, no percentile ranks, nothing that references teammates
- Statcast pitch-type color palette used everywhere, no exceptions
- Inter font throughout (via `bslib::font_google("Inter")`)
- All ggplot charts: white background, horizontal-only light gridlines, `config(displayModeBar=FALSE)` on every plotly embed
- Player view: teal `#2A9D8F` / amber `#F4A261` for scorecard tile accents — no red (`#E63946`) in player-facing outputs

---

## 1. Color Constants (`global.R`)

### Pitch-type colors — replace `PITCH_COLORS`
```r
PITCH_COLORS <- c(
  FourSeamFastBall = "#D22D49", Fastball        = "#D22D49",
  Sinker           = "#FE9D00", TwoSeamFastBall = "#FE9D00",
  Cutter           = "#933F2C",
  ChangeUp         = "#1DBE3A", Splitter        = "#3BACAC",
  Slider           = "#EEE716", Sweeper         = "#DDB33A",
  Curveball        = "#00D1ED",
  Undefined        = "#AAAAAA"
)
```

### Category colors — replace `PITCH_CATEGORY_COLORS`
```r
PITCH_CATEGORY_COLORS <- c(
  Fastball        = "#D22D49",
  `Breaking Ball` = "#00D1ED",
  Offspeed        = "#1DBE3A",
  Undefined       = "#AAAAAA"
)
```

---

## 2. Theme (`global.R` or new `theme.R`)

```r
SIDEBAR_BG   <- "#1E2A3A"
CONTENT_BG   <- "#F8F9FC"
CARD_BG      <- "#FFFFFF"
CARD_BORDER  <- "#E2E8F0"
TEXT_PRIMARY  <- "#1A202C"
TEXT_SECONDARY <- "#64748B"
ACCENT        <- "#1D4ED8"
GRID_LINE     <- "#F1F5F9"
RING_COLOR    <- "#D1D5DB"
```

Replace `bs_theme()` call in `ui.R`:
```r
bs_theme(
  bg = "#F8F9FC", fg = "#1A202C",
  primary = "#1D4ED8", secondary = "#64748B",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  version = 5
)
```

Replace `theme_seagulls()` in `global.R` with a cleaner version:
```r
theme_seagulls <- function() {
  theme_minimal(base_size = 12, base_family = "Inter") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#F1F5F9"),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(face = "bold", size = 13, color = "#1A202C"),
    plot.subtitle = element_text(size = 10, color = "#64748B"),
    axis.text     = element_text(color = "#64748B", size = 10),
    axis.title    = element_text(color = "#64748B", size = 11),
    legend.background = element_rect(fill = "white"),
    strip.background  = element_rect(fill = "#F1F5F9")
  )
}
```

Add `plotly_clean()` helper that wraps `plotly_white()` and removes the toolbar:
```r
plotly_clean <- function(p) {
  p %>%
    layout(paper_bgcolor = "white", plot_bgcolor = "white",
           font = list(color = "#1A202C", family = "Inter")) %>%
    config(displayModeBar = FALSE)
}
```

---

## 3. Navigation — Game Chip Selector

**Replaces:** `dateRangeInput("dates", ...)` in `coach_sidebar()`

**New control:** `uiOutput("game_selector")` rendered server-side as a horizontal scrollable row of action buttons. Choices derive from `sort(unique(app_data()$Date), decreasing = TRUE)`.

```
[ Season ] [ Last 5 ] [ Jun 14 ] [ Jun 7 ] [ May 31 ] …
```

**Server reactive** `selected_game_range()` returns a list `list(dates, label)`:
- "Season" → all dates
- "Last 5" → 5 most recent game dates
- individual date → that date only

**Sidebar control removed:** `dateRangeInput` entirely gone. `selectInput("count", ...)` stays. `sliderInput("innings", ...)` stays. Pitch type picker removed — replaced by `pitch_group_mode` toggle already in place.

**CSS for chips:**
```css
.game-chip { 
  display:inline-block; padding:4px 10px; margin:2px;
  border-radius:20px; border:1px solid #CBD5E1; background:#fff;
  font-size:12px; cursor:pointer; color:#1A202C;
}
.game-chip.active { background:#1E2A3A; color:#fff; border-color:#1E2A3A; }
```

---

## 4. Coach Sidebar (final shape)

```
[Logo 44px] SAN FRANCISCO SEAGULLS
Logged in as: {name}    [Log Out]
─────────────────────────────
[ Pitching ]  [ Hitting ]
─────────────────────────────
Game
[Season][Last 5][Jun 14][Jun 7]…  ← scrollable chips

Player
[All Players ▾]

Group By
[Category]  [Pitch Type]

Count
[All ▾]

Innings ══════════ 1–9
─────────────────────────────
Key Metrics box
─────────────────────────────
[⟳ Sync Data from Drive]
```

Sidebar `width: 280px`, background `#1E2A3A`, zero left margin, flush to viewport edge (`margin: 0; padding-left: 0` on body).

White card background for filter controls (same pattern as current). Sidebar header padding `16px`.

---

## 5. Coach View — Pitching Overview

**KPI tiles row:** Strike% · Whiff% · CSW% · GB% (already implemented, keep)

**Arsenal table** (full width, first chart coaches see):
Columns: `Pitch | Usage% | Avg Velo | Max Velo | Spin | IVB | HB`
This replaces `table_movement` + `table_pitchers` on Overview. Move `table_pitchers` to Detail.

**Movement profile** (new output `plot_movement_overview`, square):
- IVB (y) vs HB (x), coord_fixed()
- Reference rings at 6", 12", 18" (dashed, `#D1D5DB`)
- Crosshairs at origin (solid, `#D1D5DB`)
- Points colored by `group_col()` (category or type), alpha=0.7, size=3
- Cluster centroids labeled with pitch abbreviation
- Subtitle: "Pitcher's-eye view"

Overview tab now has: KPI glance → Arsenal table → Movement profile. That's it.

**Detail tab** keeps: Location map, Velo/Spin bars, Release Point, Outcomes, Count heatmap.

---

## 6. Coach View — Hitting Overview

**KPI tiles:** AVG · Hard Hit% · Barrel% · Avg EV (already implemented)

**Batter leaderboard table** (full width, primary chart)

**Spray chart** (new output `plot_spray_coach`):
- Overhead field outline (standard diamond geometry, foul lines, arcs at 150ft/300ft)
- Dots at `(HorzLaunchDir, Distance)` converted to (x,y) cartesian, sized by `ExitSpeed`
- Color by `TaggedHitType` (GroundBall/FlyBall/LineDrive/Popup) with `bip_colors`
- Subtitle: "Dot size = exit velocity"

Detail tab keeps: EV/LA, Swing zones, Hit types, Pitch vulnerability.

---

## 7. Player View

**Game chip selector** at top of player shell (same component, scoped to that player's game dates).

**Pitcher player:**
- Takeaway sentence (already exists)
- Scorecard tiles: Strike% · Whiff% · CSW% · GB% with trend sparklines, NO team comparison
- Movement profile (same Savant-style as coach view but player's pitches only)
- Pitch Location map (catcher's view, home plate, Statcast colors)
- Nothing else

**Batter player:**
- Takeaway sentence
- Scorecard tiles: AVG · Hard Hit% · Barrel% · Avg EV with trend sparklines, NO team comparison
- Spray chart (their BIP only)
- Swing zone heatmap (their swing rates, catcher's view)
- Nothing else

No release point, no pitch vulnerability heatmap, no pitch outcomes bar — those are coach tools.

---

## 8. Movement Profile — Savant-Style Reference Rings

Add to both `plot_movement` (coach) and the new player movement chart:

```r
# Reference rings
ring_df <- function(r, n=200) {
  theta <- seq(0, 2*pi, length.out=n)
  data.frame(x=r*cos(theta), y=r*sin(theta), r=r)
}
rings <- bind_rows(lapply(c(6,12,18), ring_df))

# In ggplot:
geom_path(data=rings, aes(x=x, y=y, group=r),
          color="#D1D5DB", linetype="dashed", linewidth=0.4,
          inherit.aes=FALSE)
```

---

## 9. Spray Chart Geometry

Convert TrackMan's `Bearing` (launch direction, degrees from center) and `Distance` to cartesian:

```r
spray_xy <- function(bearing_deg, distance_ft) {
  rad <- bearing_deg * pi / 180
  list(x = distance_ft * sin(rad),
       y = distance_ft * cos(rad))
}
```

Field outline drawn with `annotate("path", ...)` for foul lines (45° from center) and arc segments.

---

## 10. What Does NOT Change

- `credentials.sqlite` / auth flow / `shinymanager` setup
- Metric functions in `global.R` (`strike_pct`, `whiff_pct`, etc.)
- `.secrets/` OAuth, `data/game_csvs/`, `.Renviron` (all gitignored)
- Drive sync handler
- `testthat` test suite — tests remain green
- The "no red in player view" rule

---

## Self-Review

- No TBDs or vague sections
- Section 10 explicitly lists what doesn't change — no scope creep
- Player view team-comparison removal is explicit and repeated in Global Constraints
- Spray chart geometry is fully specified (bearing + distance conversion)
- Color constants are exact hex values, no ambiguity
- Game chip selector server logic is fully described
