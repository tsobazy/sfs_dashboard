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
| **AVG** | Hits ÷ AB, where Hits = Single+Double+Triple+HomeRun and **AB = PA − BB − HBP − Sacrifice** | `PlayResult`, `KorBB`, `PitchCall`, `Date`, `Inning`, `PAofInning` |
| **OBP** | (H + BB + HBP) ÷ PA | `PlayResult`, `KorBB`, `PitchCall` |
| **SLG** | total bases (1B+2·2B+3·3B+4·HR) ÷ AB | `PlayResult` |
| **Avg EV** | mean of `ExitSpeed` on balls in play | `ExitSpeed`, `PitchCall` |
| **Hard Hit%** | share of balls in play with `ExitSpeed ≥ 85` mph (`HARD_HIT_MPH`) — calibrated to this college wood-bat league (avg EV ~79 mph) | `ExitSpeed` |
| **Barrel%** | share of balls in play with `ExitSpeed ≥ 90` mph **and** launch angle `Angle` between 20°–35° (`BARREL_MPH` / `BARREL_LA_LOW` / `BARREL_LA_HI`) | `ExitSpeed`, `Angle` |
| **Zone Swing%** | swings on in-zone pitches ÷ in-zone pitches | `PitchCall`, `PlateLocSide`, `PlateLocHeight` |
| **PA** | distinct plate appearances = `n_distinct(Date + Inning + PAofInning)` | `Date`, `Inning`, `PAofInning` |
| **H** | Single + Double + Triple + HomeRun | `PlayResult` |
| **BB** | `KorBB == "Walk"` | `KorBB` |

---

## Evaluation benchmarks (tile colors)

The green/amber tile colors grade each player against league-realistic bars,
derived from the **actual distribution** in the current data (all teams = the
league), roughly green ≈ top quartile, amber ≈ bottom quartile. These are
**provisional** on ~8 games and will firm up as more data arrives.

| Metric | Green (good) ≥ | Amber (poor) ≤ | Direction |
|--------|----------------|----------------|-----------|
| Strike% | 0.65 | 0.53 | higher better |
| Whiff% | 0.29 | 0.13 | higher better |
| CSW% | 0.31 | 0.21 | higher better |
| Chase% (pitcher induces) | 0.30 | 0.20 | higher better |
| GB% | 0.48 | 0.33 | higher better |
| AVG | .300 | .220 | higher better |
| Avg EV | 84 mph | 73 mph | higher better |
| Hard Hit% | 0.50 | 0.30 | higher better |
| Barrel% | 0.12 | 0.04 | higher better |
| Zone Swing% | 0.78 | 0.60 | higher better |
| Chase% (hitter discipline) | 0.22 | 0.32 | **lower** better |

## ⚠️ Known simplifications (read before trusting against "official" stats)

These are honest deviations from textbook definitions — not bugs, but worth knowing:

1. **Barrel% is a simplified, league-calibrated proxy.** MLB's official barrel is
   a *sliding scale* at much higher exit velos (≥98 mph). At this league's EVs that
   yields ~0 barrels, so we use a fixed EV ≥ 90 AND LA 20–35° (~8% league rate). It
   rewards genuinely hard, well-launched contact for this level, not MLB barrels.
2. **AB now excludes BB, HBP, and Sacrifice** (catcher's interference, if it ever
   appears, is not handled). OBP includes HBP in the numerator. The OBP denominator
   uses PA (it does not separate sac flies from sac bunts — negligible at this
   data's volume).
3. **Benchmarks come from a small sample** (~8 games) and include opponents as the
   league reference. They are provisional and should be re-derived as data grows.
4. **BF / PA counting** assumes `PAofInning` uniquely identifies a plate
   appearance within an inning of a game. Re-thrown/duplicated rows in a raw
   TrackMan file could affect the count.
5. **Strike zone is a fixed rulebook box**, not per-batter. Chase%/Zone% use the
   same box for everyone regardless of height/stance.
6. **No minimum-sample gate yet.** Small samples can show extreme rates. A 30-AB
   (and pitcher-equivalent) minimum is planned but not yet applied.

_Last updated: 2026-06-25._
