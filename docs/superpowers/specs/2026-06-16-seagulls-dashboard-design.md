# SF Seagulls TrackMan Dashboard — Design Spec
**Date:** 2026-06-16  
**Project:** `/Users/tsobazy/sfs_dashboard`  
**Target:** R Shiny app deployable via `shiny::runApp()` and `shinyapps.io`

---

## Overview

A white-background, two-tab Shiny dashboard for the San Francisco Seagulls (CCL) using TrackMan pitch-by-pitch data from Fall 2025. Coaches and players use it to review pitching and hitting performance across 14 interactive charts.

---

## File Structure

```
sfs_dashboard/
├── global.R        # packages, data load, constants, metric functions, theme
├── ui.R            # layout: navbar, sidebar, Pitching tab, Hitting tab
├── server.R        # all reactive logic, chart renders, insight panel
└── all_fall_25.csv # 4,639-row TrackMan export (must be in root at runtime)
```

---

## Data

- **File:** `all_fall_25.csv` — loaded relative to app root
- **Rows:** 4,639, one per pitch
- **Key columns used:**

| Column | Type | Notes |
|---|---|---|
| Pitcher, Batter | chr | Player names |
| PitcherTeam, BatterTeam | chr | Team codes |
| PitcherThrows, BatterSide | chr | Handedness |
| Date | date | `%Y-%m-%d` |
| Inning, Balls, Strikes | int | |
| TaggedPitchType | chr | Drop "Undefined" rows from charts |
| RelSpeed | dbl | mph |
| SpinRate | dbl | rpm |
| InducedVertBreak, HorzBreak | dbl | inches |
| RelHeight, RelSide | dbl | feet |
| PlateLocHeight, PlateLocSide | dbl | feet at plate |
| PitchCall | chr | See outcome buckets below |
| KorBB | chr | `"Strikeout"` / `"Walk"` / `"Undefined"` |
| TaggedHitType | chr | GroundBall / FlyBall / LineDrive / Popup |
| PlayResult | chr | Single / Double / Triple / HomeRun / Out / Error / Undefined |
| ExitSpeed, Angle, Distance, Direction | dbl | InPlay pitches only |

**PitchCall values:** `BallCalled`, `StrikeCalled`, `StrikeSwinging`, `FoulBallNotFieldable`, `FoulBallFieldable`, `InPlay`, `HitByPitch`, `BallinDirt`

**Data prep in `global.R`:**
```r
data$TaggedPitchType <- if_else(
  data$TaggedPitchType %in% c("Other", NA_character_), "Undefined", data$TaggedPitchType
)
data$Count <- paste0(data$Balls, "-", data$Strikes)
```

---

## Constants (global.R)

```r
SZ_LEFT  <- -0.83
SZ_RIGHT <-  0.83
SZ_BOT   <-  1.50
SZ_TOP   <-  3.50

PITCH_COLORS <- c(
  FourSeamFastBall = "#E63946", Sinker       = "#F4A261",
  ChangeUp         = "#2A9D8F", Curveball    = "#457B9D",
  Slider           = "#6A4C93", Sweeper      = "#9B2226",
  Cutter           = "#E9C46A", Splitter     = "#264653",
  Fastball         = "#E76F51", TwoSeamFastBall = "#F4D35E",
  Undefined        = "#AAAAAA"
)
```

---

## Derived Metric Functions (global.R)

All return a single `double` (0–1 or `NA_real_`). Used in both server renders and the insight panel.

| Function | Formula |
|---|---|
| `strike_pct(calls)` | `mean(calls %in% c("StrikeCalled","StrikeSwinging","FoulBall*","InPlay"))` |
| `whiff_pct(calls)` | `sum(calls=="StrikeSwinging") / sum(swings)` — returns NA if 0 swings |
| `csw_pct(calls)` | `mean(calls %in% c("StrikeCalled","StrikeSwinging"))` |
| `chase_pct(side, height, calls)` | swings on OOZ pitches / total OOZ pitches |
| `hard_hit_pct(ev)` | `mean(ev >= 95)` |
| `barrel_pct(ev, la)` | `mean(ev >= 98 & la >= 26 & la <= 30)` |
| `gb_pct(ht)` | `sum(ht=="GroundBall") / sum(ht %in% bip_types)` |

---

## Theme (global.R)

White background throughout. Applied to every ggplot2 chart.

```r
theme_seagulls <- function() {
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#e8e8e8"),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, color = "#0a1628"),
    plot.subtitle    = element_text(size = 11, color = "#555555"),
    axis.text        = element_text(color = "#333333"),
    legend.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "#f0f0f0")
  )
}
```

Every `ggplotly()` call ends with:
```r
%>% layout(paper_bgcolor="white", plot_bgcolor="white", font=list(color="#0a1628"))
```

bslib theme in `ui.R`:
```r
bs_theme(bg="white", fg="#0a1628", primary="#0a1628")
```

---

## Packages

```r
shiny, bslib, shinyWidgets, tidyverse, lubridate, plotly, DT, scales
```

---

## UI Layout (ui.R)

- `fluidPage` with bslib theme
- Fixed left sidebar (280px) with all global filters + insight box
- Right content: `tabsetPanel` with 2 tabs

### Sidebar Filters (input IDs match spec exactly)

| ID | Widget | Description |
|---|---|---|
| `view_mode` | `radioGroupButtons` | "Pitching" / "Hitting" |
| `player` | `pickerInput` | Dynamic — updates on view_mode change; "All Players" default |
| `dates` | `dateRangeInput` | Full season range default |
| `pitch_types` | `pickerInput` | Multi-select, all selected by default, actions-box=TRUE |
| `count` | `selectInput` | "All" + all 12 counts |
| `innings` | `sliderInput` | 1–9, both endpoints |

Player dropdown populates from `data$Pitcher` (Pitching mode) or `data$Batter` (Hitting mode), updated via `updatePickerInput` in an `observe()`.

Below filters: `uiOutput("insights")` — reactive insight box.

---

## Master Filtered Reactive (server.R)

Single `fdata <- reactive({...})` drives all 14 charts.

Filters applied in order:
1. Date range (`dates[1]` to `dates[2]`)
2. Pitch types (`TaggedPitchType %in% input$pitch_types`)
3. Exclude Undefined pitch types
4. Count (split `"B-S"` string → filter `Balls` and `Strikes`)
5. Inning range
6. Player (Pitcher or Batter depending on `view_mode`)

Guards: `req(nrow(fdata()) > 0)` at top of every `render*()`.

---

## Tab 1 — Pitching (8 charts)

### Chart 1: Strike Zone Map
- `geom_point(aes(x=PlateLocSide, y=PlateLocHeight, color=TaggedPitchType))`
- Strike zone rect: `annotate("rect", ...)` — black outline, no fill
- x: −2.5 to 2.5 | y: 0 to 5 | alpha 0.55, size 2
- Tooltip: Pitcher, Pitch Type, Speed (mph), Result
- Title: "Pitch Location Map" | Subtitle: "Each dot is one pitch. The black box is the strike zone."

### Chart 2: Pitch Arsenal Donut
- `plot_ly(type="pie", hole=0.5)`
- Colors from `PITCH_COLORS`
- Hover: pitch type, count, %, avg RelSpeed, avg SpinRate

### Chart 3: Velocity & Spin by Pitch Type
- `plotly::subplot()` side-by-side (not patchwork — incompatible with ggplotly)
- Left panel: avg RelSpeed per pitch type (bar, `coord_flip`)
- Right panel: avg SpinRate per pitch type (bar, `coord_flip`)
- Fill by `PITCH_COLORS`; bar value labels via `geom_text(hjust=-0.1)`

### Chart 4: Pitcher Leaderboard Table
- `DT::datatable()`, one row per pitcher
- Columns: Pitcher | Pitches | Strike% | Whiff% | CSW% | Avg Velo | K% | BB% | GB%
- K% and BB% derived from `KorBB` column (`"Strikeout"` / `"Walk"`)
- `formatPercentage()` on pct cols, `formatRound()` on Avg Velo
- Strike% color: ≥65% green, ≤54% red | Whiff% color: ≥30% green, ≤19% red
- `options=list(pageLength=10, order=list(list(2,"desc")))`

### Chart 5: Pitch Movement Scatter
- Average per pitch type (not individual pitches — too crowded at 4,639 rows)
- `geom_point(aes(x=HorzBreak, y=InducedVertBreak, color=TaggedPitchType))`
- Crosshair at 0,0 | Quadrant labels: "Rise", "Arm-Side Run", "Drop", "Glove-Side Break"
- Size scaled to pitch count (`scale_size_continuous`)
- Title: "How Much Each Pitch Moves"

### Chart 6: Release Point Scatter
- Individual points: `geom_point(aes(x=RelSide, y=RelHeight, color=TaggedPitchType), alpha=0.5)`
- Ellipses: `stat_ellipse(aes(group=TaggedPitchType, color=TaggedPitchType))`
- Title: "Release Point" | Subtitle: "Tighter clusters = more consistent mechanics"

### Chart 7: Pitch Outcome Stacked Bar
- `PitchCall` mapped to 5 buckets: Ball | Called Strike | Foul | Whiff | In Play
- `position="fill"` (proportion)
- x-axis sorted by CSW% descending
- Title: "What Happened on Each Pitch Type"

### Chart 8: Count Heatmap
- Strike% for every `Balls × Strikes` combination (12 cells)
- `geom_tile` + `geom_text` with percentage labels
- `scale_fill_gradient2(low="#E63946", mid="white", high="#2DC653", midpoint=0.60)`
- Title: "Strike% by Count"

---

## Tab 2 — Hitting (6 charts)

### Chart 9: Spray Chart
- Filter: `PlayResult` not "Undefined", `Direction` not NA
- `spray_x = Distance * sin(Direction * pi/180)`, `spray_y = Distance * cos(Direction * pi/180)`
- Field outline drawn with `geom_segment` + `geom_curve` in gray
- `coord_fixed()`
- Colors: Single="#2DC653", Double="#F5C518", Triple="#FF8C00", HomeRun="#E63946", Out="#AAAAAA"
- Title: "Where the Ball Was Hit"

### Chart 10: Exit Velocity vs. Launch Angle
- Filter: `InPlay` pitches with non-NA `ExitSpeed` and `Angle`
- `geom_point(aes(x=Angle, y=ExitSpeed, color=PlayResult), alpha=0.7)`
- Barrel zone: `annotate("rect", xmin=10, xmax=30, ymin=95, ymax=Inf, fill="#2DC653", alpha=0.1)`
- Label at (20, 117): "Barrel Zone"
- Title: "Exit Velocity & Launch Angle"

### Chart 11: Batter Leaderboard Table
- Columns: Batter | PA | AVG | OBP | SLG | K% | BB% | Avg EV | Hard Hit% | Barrel%
- AVG/OBP/SLG computed from `PlayResult` and `KorBB` at PA level:
  - PA = `n_distinct(paste(Date, Inning, PAofInning))`
  - H = rows where `PlayResult %in% c("Single","Double","Triple","HomeRun")`
  - BB = rows where `KorBB == "Walk"` | K = rows where `KorBB == "Strikeout"`
  - AB = PA − BB (simplified; no SF data available)
  - TB = 1×Single + 2×Double + 3×Triple + 4×HomeRun
  - AVG = H / AB | OBP = (H + BB) / PA | SLG = TB / AB
- Avg EV: ≥92 mph = green, ≤82 mph = red
- `formatPercentage()` on pct cols, `formatRound()` on Avg EV

### Chart 12: Swing Decision Heatmap
- 9-zone 3×3 grid inside the strike zone (equal thirds of width and height)
- Per zone: `Swing% = swings / pitches seen`
- `geom_tile` colored by Swing% | `geom_text` with percentage
- Title: "Swing Rates by Zone" | Subtitle: "Do hitters swing at the right pitches?"

### Chart 13: Hit Type Distribution
- `geom_bar(aes(x=Batter, fill=TaggedHitType), position="fill") + coord_flip()`
- Colors: GroundBall="#8B4513", FlyBall="#457B9D", LineDrive="#2DC653", Popup="#AAAAAA"
- Title: "Batted Ball Type by Hitter"

### Chart 14: Pitch Vulnerability Heatmap
- Rows = batters, columns = pitch types
- Value = Whiff% vs. pitch type; cells with <5 pitches shown as NA/gray
- `scale_fill_gradient2(low="#2DC653", mid="white", high="#E63946", midpoint=0.25)`
- Title: "Batter Whiff Rate vs. Each Pitch Type"

---

## Insight Box (sidebar, below filters)

Reactive `renderUI` showing:
- Strike%, Whiff%, CSW%, Chase% for current filter selection
- Best whiff pitch (≥10 pitches, highest Whiff%)

---

## Error Handling

- All `renderPlotly` / `renderDataTable` calls wrap with `req(nrow(fdata()) > 0)`
- Charts return `validate(need(...))` messages for empty filtered subsets

---

## Deployment Notes

- App runs via `shiny::runApp("/Users/tsobazy/sfs_dashboard")`
- `all_fall_25.csv` must be in the project root at runtime
- All packages must be installed; no local path dependencies beyond the CSV
- No changes needed between local run and `shinyapps.io` deploy (relative CSV path)

---

## Resolved Spec Gaps

| Gap | Resolution |
|---|---|
| `whiff_pct` function body truncated | `sum(calls=="StrikeSwinging") / sum(swings)` |
| `element_re(` typo in theme | `element_rect(` |
| Chart 3 mentions patchwork | Use `plotly::subplot()` (ggplotly-compatible) |
| Chart 7 `x=TaggedPitch` truncated | `x=TaggedPitchType` |
| Player dropdown code truncated | `c("All Players", sort(unique(data$Batter)))` |
| `RelHei` in column list | Actual column is `RelHeight` (confirmed in CSV headers) |
| Slider pitch color | Using `#6A4C93` per spec |
| AVG/OBP/SLG in batter table | Computed from `PlayResult` + `KorBB` at PA level |
