# SF Seagulls Dashboard — User Roles & Auth Design Spec
**Date:** 2026-06-16
**Project:** `/Users/tsobazy/sfs_dashboard`
**Extends:** `2026-06-16-seagulls-dashboard-design.md`

---

## Overview

Add role-based login to the existing dashboard so coaches see the full analytical tool while players see a personal, process-focused view of their own stats. Built for post-game use: coaches on a laptop reviewing the team, players on a phone reflecting on their own performance.

---

## File Structure

```
sfs_dashboard/
├── global.R          # + shinymanager setup, source("roster.R")
├── roster.R          # NEW — credentials data frame + position lookup table
├── ui.R              # + shinymanager wrapper, coach/player layout switch
├── server.R          # + role-based reactive routing, player auto-filter
└── all_fall_25.csv
```

`roster.R` is sourced at the top of `global.R`. Keeping credentials separate means the roster can be updated (passwords, new players) without touching core app logic.

---

## Authentication

**Package:** `shinymanager`

`shinymanager::secure_app()` wraps the entire `ui` definition. On app load, users see a login screen before any data is shown. After successful login, `shinymanager` passes the authenticated user's row into the app via `shinymanager::result_auth`.

### Credentials Schema (`roster.R`)

```r
credentials <- data.frame(
  user        = character(),   # e.g. "bryce_brooks", "coach_cascone"
  password    = character(),   # default = jersey number (string); coaches set own
  role        = character(),   # "player" | "coach"
  player_name = character(),   # exact match to Pitcher/Batter in TrackMan data; NA for coaches
  player_type = character(),   # "pitcher" | "hitter" | "two-way" | NA for coaches
  stringsAsFactors = FALSE
)
```

### Full Roster Credentials

#### Coaches (role = "coach")
| user | display name |
|---|---|
| `coach_cascone` | Cage Cascone |
| `coach_dumlao` | Dominic Dumlao |
| `coach_ferreira` | Bill Ferreira |
| `coach_medina` | Andres Medina |
| `coach_aranibar` | Jorge Aranibar |
| `coach_ballelos` | Eric Ballelos |
| `coach_caviglia` | Marc Caviglia |
| `coach_frediani` | Danielle Frediani |
| `coach_rodarte` | Ashley Rodarte |

#### Players (role = "player")
| # | user | player_name | player_type |
|---|---|---|---|
| 1 | `bryce_brooks` | Bryce Brooks | hitter |
| 3 | `sebastian_ultreras` | Sebastian Ultreras | hitter |
| 6 | `declan_mendel` | Declan Mendel | pitcher |
| 7 | `emilio_feliciano` | Emilio Feliciano | hitter |
| 9 | `davis_germann` | Davis Germann | hitter |
| 12 | `theodore_tsouras` | Theodore Tsouras | pitcher |
| 13 | `benjamin_joost` | Benjamin Joost | pitcher |
| 14 | `finn_whalen` | Finn Whalen | pitcher |
| 16 | `matthew_potter` | Matthew Potter | pitcher |
| 17 | `louden_hilliard` | Louden Hilliard | pitcher |
| 20 | `caid_heflin` | Caid Heflin | hitter |
| 22 | `jacob_gilbreath` | Jacob Gilbreath | hitter |
| 25 | `joseph_steidel` | Joseph Steidel | pitcher |
| 26 | `blake_cowans` | Blake Cowans | hitter |
| 29 | `jake_brewer` | Jake Brewer | hitter |
| 30 | `ethan_lopez` | Ethan Lopez | hitter |
| 31 | `caleb_garrison` | Caleb Garrison | pitcher |
| 32 | `marcus_graham` | Marcus Graham | hitter |
| 34 | `connor_wood` | Connor Wood | pitcher |
| 35 | `taylor_easthope` | Taylor Easthope | pitcher |
| 36 | `derek_waldvogel` | Derek Waldvogel | hitter |
| 37 | `tanner_wall` | Tanner Wall | hitter |
| 38 | `armando_hurtado` | Armando Hurtado | hitter |
| 39 | `brandon_swanson` | Brandon Swanson | hitter |
| 40 | `christian_lamothe` | Christian LaMothe | hitter |
| 43 | `jb_ferreira` | JB Ferreira | pitcher |
| 50 | `kai_hanasaki` | Kai Hanasaki | pitcher |
| 53 | `branson_derrington` | Branson Derrington | pitcher |
| 54 | `camren_boyd` | Camren Boyd | pitcher |
| 55 | `luka_shah` | Luka Shah | pitcher |
| 56 | `alan_ramirez` | Alan Ramirez | hitter |

Default player password = jersey number as a string (e.g. `"12"` for Theodore Tsouras). Default coach password = `"seagulls2026"`. All users can change their password via `shinymanager`'s built-in password-change screen (accessible from the login page).

### Position Lookup Table (`roster.R`)

Used to add a Position column to coach leaderboard tables.

```r
roster_positions <- data.frame(
  player_name = character(),  # matches Pitcher/Batter in TrackMan
  position    = character(),  # e.g. "RHP", "LHP", "INF", "OF", "C", "1B/OF"
  number      = integer(),
  stringsAsFactors = FALSE
)
```

Full entries mirror the 2026 roster. Joined onto leaderboard summaries in `server.R` before passing to `DT::datatable()`.

---

## Role-Based View Routing

In `server.R`, after `shinymanager::result_auth` is available:

```r
user_role <- reactive({
  result_auth()$role          # "coach" | "player"
})
user_player_name <- reactive({
  result_auth()$player_name   # NA for coaches
})
user_player_type <- reactive({
  result_auth()$player_type   # "pitcher" | "hitter" | "two-way" | NA
})
```

`ui.R` uses `uiOutput("main_ui")` inside the `shinymanager` wrapper. `server.R` renders either the coach layout or the player layout based on `user_role()`.

---

## Coach View

The full existing dashboard — all 14 charts, all sidebar filters — with two additions:

1. **Position column** added to both leaderboard tables (Chart 4: Pitcher Leaderboard, Chart 11: Batter Leaderboard). Joined from `roster_positions` by player name. Column appears second (after Name, before Pitches/PA).

2. **Header bar** at top of sidebar: `"Logged in as: [display name] (Coach)"` + a `actionButton("logout", "Log Out")` that calls `session$reload()` to return to the login screen.

No other changes to the coach view.

---

## Player View

A separate layout rendered when `user_role() == "player"`. The player cannot access any other player's data — `fdata()` is pre-filtered to `player_name` and the filter is not exposed in the UI.

### Layout

```
┌─────────────────────────────────────┐
│  [Team logo / name]  Logged in as:  │
│  [Player Name] (#XX) · [Pos]        │
│                          [Log Out]  │
├─────────────────────────────────────┤
│  LAST GAME  [date]   [← Prev Game] [Next Game →]  │
│  ┌──────────────────────────────────────────────┐ │
│  │  Scorecard: 3–4 large stat tiles             │ │
│  └──────────────────────────────────────────────┘ │
├─────────────────────────────────────┤
│  Charts (pitcher section and/or hitter section)   │
└─────────────────────────────────────┘
```

- No sidebar. Navigation is a game browser (prev/next game buttons + date label).
- `bslib` responsive columns — charts stack vertically on narrow screens (mobile-friendly).
- The "last game" is the most recent date in the data where the player appears. On first load, this game is selected automatically.

### Game Browser

```r
player_games <- reactive({
  col <- if (user_player_type() == "pitcher") "Pitcher" else "Batter"
  data %>%
    filter(.data[[col]] == user_player_name()) %>%
    pull(Date) %>%
    unique() %>%
    sort(decreasing = TRUE)
})
```

Prev/Next buttons shift a `selected_game_index` reactive integer through `player_games()`.

### Player `fdata()`

Filters the full dataset to the selected game and the logged-in player only. Excludes Undefined pitch types. No other filters exposed.

---

## Player View — Pitcher Section

Shown when `player_type %in% c("pitcher", "two-way")`.

### Scorecard (last game, 4 tiles)
| Tile | Metric | Color threshold |
|---|---|---|
| Strike% | `strike_pct(PitchCall)` | ≥65% teal, ≤54% amber |
| Whiff% | `whiff_pct(PitchCall)` | ≥30% teal, ≤19% amber |
| CSW% | `csw_pct(PitchCall)` | ≥28% teal, ≤20% amber |
| Chase% | `chase_pct(PlateLocSide, PlateLocHeight, PitchCall)` | ≥30% teal |

Colors use teal (`#2A9D8F`) for strong performance, amber (`#F4A261`) for areas to watch. No red — framing is developmental, not judgmental.

### Charts (4)

| Chart | Description |
|---|---|
| Pitch Location Map | Same as Chart 1 from main spec — their pitches only |
| Pitch Arsenal | Donut chart of pitch mix — same as Chart 2 |
| Release Point | Scatter + ellipses — same as Chart 6 |
| Pitch Outcome Breakdown | Stacked bar of call outcomes by pitch type — same as Chart 7 |

No velocity/spin leaderboard, no count heatmap — those are coach-level analytical charts.

---

## Player View — Hitter Section

Shown when `player_type %in% c("hitter", "two-way")`.

### Scorecard (last game, 4 tiles)
| Tile | Metric | Color threshold |
|---|---|---|
| Avg Exit Velocity | `mean(ExitSpeed, na.rm=TRUE)` on InPlay pitches | ≥92 mph teal, ≤82 mph amber |
| Hard Hit% | `hard_hit_pct(ExitSpeed)` | ≥40% teal, ≤25% amber |
| Swing Zone% | Swing% on pitches inside the strike zone | ≥70% teal |
| Chase% | Swing% on pitches outside the strike zone | ≤25% teal, ≥35% amber |

### Charts (3)

| Chart | Description |
|---|---|
| Spray Chart | Where the ball was hit — same as Chart 9 |
| Swing Decision Heatmap | 3×3 zone swing rates — same as Chart 12 |
| Hit Type Distribution | GB/FB/LD/Popup breakdown — same as Chart 13 (single player, not multi-batter) |

No AVG, OBP, SLG, K%, BB% — outcome stats are excluded from the player view.

---

## Two-Way Players

If `player_type == "two-way"`, both sections render on the same page, pitcher section first, hitter section below, separated by a divider. The game browser filters both sections simultaneously.

No two-way players are identified on the current 2026 roster. The field is reserved for future seasons.

---

## Mobile Responsiveness (Player View Only)

The player view uses `bslib::layout_columns()` with `col_widths = breakpoints(sm = 12, md = 6)` so:
- On phones (< 768px): scorecard tiles and charts each take full width, stacked
- On tablets/desktop: tiles appear in a row, charts in 2-column grid

Coach view is desktop-only (no change from existing spec).

---

## Error Handling

- If a player has no data for the selected game: scorecard shows "—" in all tiles, charts display `validate(need(..., "No data for this game."))`.
- If a player appears in the roster but never in the TrackMan data: game browser shows empty, with a message "No games recorded yet."
- All existing `req(nrow(fdata()) > 0)` guards remain in place.

---

## What Is Not Changing

Everything in `2026-06-16-seagulls-dashboard-design.md` remains authoritative for the coach view. This spec only adds:
- `roster.R` (new file)
- Auth layer (`shinymanager`)
- Player view layout and charts
- Position column in coach leaderboards

No existing chart logic, constants, theme, or metric functions change.
