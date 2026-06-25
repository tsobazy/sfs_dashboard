# Metrics Reference — how every number is calculated

This documents exactly how each stat in the dashboard is computed from the
TrackMan CSV columns, so the numbers can be audited. Formulas live in
`global.R` (helper functions) and `server.R` (per-chart aggregation).

## Data source & filtering

- Data is the combined real game CSV (`all_fall_25.csv`, rebuilt from the Drive
  game files). Each row is **one pitch**.
- Only **Seagulls** players are shown: rows are filtered to `PitcherTeam == "SAN_FRA4"`
  on the Pitching tab and `BatterTeam == "SAN_FRA4"` on the Hitting tab
  (`SEAGULLS_TEAM` in `global.R`).
- The sidebar filters (games window, pitch category, count, innings) subset the
  rows before any metric is computed.

## Pitch grouping (`PITCH_CATEGORY_MAP`, global.R)

`TaggedPitchType` from TrackMan is collapsed into four categories:

| Category | TaggedPitchType values |
|----------|------------------------|
| **Fastball** | FourSeamFastBall, Fastball, Sinker, TwoSeamFastBall, Cutter |
| **Breaking Ball** | Slider, Curveball, Sweeper |
| **Offspeed** | ChangeUp, Splitter |
| **Undefined** | anything else / "Other" / blank (excluded from most charts) |

## Strike zone (used by Chase% and zone charts)

A rulebook box, in feet, from the catcher's perspective (`global.R`):
`SZ_LEFT = -0.83`, `SZ_RIGHT = 0.83`, `SZ_BOT = 1.50`, `SZ_TOP = 3.50`.
A pitch is "out of zone" if `PlateLocSide`/`PlateLocHeight` falls outside that box.

## Count buckets (sidebar "Count" filter)

- **Pitcher's Count:** 0-1, 0-2, 1-2, 2-2
- **Hitter's Count:** 1-0, 2-0, 2-1, 3-0, 3-1
- **2K (two-strike):** 0-2, 1-2, 2-2, 3-2

`Count` is built as `paste0(Balls, "-", Strikes)`.

---

## Pitching metrics

All operate on the `PitchCall` column. A **swing** = `PitchCall` in
{StrikeSwinging, FoulBallNotFieldable, FoulBallFieldable, InPlay}.

| Metric | Formula | CSV columns |
|--------|---------|-------------|
| **Strike%** | (StrikeCalled + StrikeSwinging + both Foul types + InPlay) ÷ all pitches | `PitchCall` |
| **Whiff%** | StrikeSwinging ÷ swings | `PitchCall` |
| **CSW%** | (StrikeCalled + StrikeSwinging) ÷ all pitches | `PitchCall` |
| **Chase%** | swings on out-of-zone pitches ÷ out-of-zone pitches | `PitchCall`, `PlateLocSide`, `PlateLocHeight` |
| **Avg Velo** | mean of `RelSpeed` | `RelSpeed` |
| **Avg IVB** | mean of `InducedVertBreak` | `InducedVertBreak` |
| **Avg HB** | mean of `HorzBreak` | `HorzBreak` |
| **BF (batters faced)** | count of distinct plate appearances = `n_distinct(Date + Inning + PAofInning)` | `Date`, `Inning`, `PAofInning` |
| **K** | rows where `KorBB == "Strikeout"` | `KorBB` |
| **BB** | rows where `KorBB == "Walk"` | `KorBB` |
| **GB%** | GroundBall ÷ all balls in play (GroundBall, FlyBall, LineDrive, Popup) | `TaggedHitType` |

---

## Hitting metrics

Batted-ball metrics use only balls in play with a measured exit velocity:
`PitchCall == "InPlay" & !is.na(ExitSpeed)`.

| Metric | Formula | CSV columns |
|--------|---------|-------------|
| **AVG** | total Hits ÷ total AB, where Hits = Single+Double+Triple+HomeRun and **AB = PA − BB** | `PlayResult`, `KorBB`, `Date`, `Inning`, `PAofInning` |
| **Avg EV** | mean of `ExitSpeed` on balls in play | `ExitSpeed`, `PitchCall` |
| **Hard Hit%** | share of balls in play with `ExitSpeed ≥ 95` mph | `ExitSpeed` |
| **Barrel%** | share of balls in play with `ExitSpeed ≥ 98` mph **and** launch angle `Angle` between 26°–30° | `ExitSpeed`, `Angle` |
| **Zone Swing%** | swings on in-zone pitches ÷ in-zone pitches | `PitchCall`, `PlateLocSide`, `PlateLocHeight` |
| **PA** | distinct plate appearances = `n_distinct(Date + Inning + PAofInning)` | `Date`, `Inning`, `PAofInning` |
| **H** | Single + Double + Triple + HomeRun | `PlayResult` |
| **BB** | `KorBB == "Walk"` | `KorBB` |

---

## ⚠️ Known simplifications (read before trusting against "official" stats)

These are honest deviations from textbook definitions — not bugs, but worth knowing:

1. **Barrel% is a simplified proxy.** MLB's official barrel is a *sliding scale*
   (the qualifying launch-angle band widens as exit velo rises, ~98 mph at 26–30°
   up to a wider band at higher speeds). This app uses a **fixed** rule:
   EV ≥ 98 AND LA 26–30°. It approximates barrels but will under-count hard-hit
   balls outside that narrow angle band.
2. **AB = PA − BB only.** True at-bats also exclude hit-by-pitch, sacrifices, and
   catcher's interference. If those events exist in the data, AVG will be slightly
   off (denominator a touch too high). HBP/SAC are not currently subtracted.
3. **Hard Hit% threshold (95 mph)** and **Barrel angle band** are MLB benchmarks —
   they are *not* adjusted for this league's level. See the "league comparison"
   plan for the intended fix.
4. **BF / PA counting** assumes `PAofInning` uniquely identifies a plate
   appearance within an inning of a game. Re-thrown/duplicated rows in a raw
   TrackMan file could affect the count.
5. **Strike zone is a fixed rulebook box**, not per-batter. Chase%/Zone% use the
   same box for everyone regardless of height/stance.
6. **No minimum-sample gate yet.** Small samples can show extreme rates. A 30-AB
   (and pitcher-equivalent) minimum is planned but not yet applied.

_Last updated: 2026-06-25._
