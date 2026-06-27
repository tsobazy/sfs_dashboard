# Filename Dates + Schedule Coverage Implementation Plan

> **For agentic workers:** Implement task-by-task with TDD. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Trust CSV filename dates as the source of truth for all game dates, and live-fetch the schedule to flag scheduled games with no CSV (coach tab + player note).

**Architecture:** A filename-date override at ingestion (`build_combined_csv`) so all downstream dates are reliable. A new pure-parse + thin-fetch `schedule.R` module, joined to the CSV files present by a `build_coverage()` helper. Coach tab + player note consume the coverage result. All external steps fail soft.

**Tech Stack:** R, Shiny, bslib, dplyr, readr, rvest, testthat.

## Global Constraints

- Game date is parsed from the filename prefix `^(\d{8})-`; a file lacking it keeps its internal `Date` and logs a warning. Never silently drop.
- Schedule fetch/parse is wrapped so failure yields `NULL` → "unavailable"; the app never crashes on it.
- Player-side missing-CSV note is **team-level** (scheduled played games with no CSV), not per-player.
- `SCHEDULE_URL = "https://collegeseagulls.com/sports/bsb/2026/schedule"`.

---

### Task 1: Filename-date override in `build_combined_csv`

**Files:**
- Modify: `sync_drive.R` (`build_combined_csv`)
- Test: `tests/testthat/test-sync.R`

**Interfaces:**
- Produces: `build_combined_csv()` writes a combined CSV whose `Date` column equals each row's source-file filename date.

- [ ] **Step 1: Failing test** — append to `test-sync.R`:
```r
test_that("build_combined_csv sets Date from the filename prefix", {
  tmp <- tempdir(); game_dir <- file.path(tmp, "fdates"); dir.create(game_dir, showWarnings = FALSE)
  out <- file.path(tmp, "fd.csv")
  # internal Date deliberately wrong; filename says 2026-06-05
  mini_csv(file.path(game_dir, "20260605-SanBrunoPark-1_unverified.csv"), "Jones, Bob", n = 3)
  build_combined_csv(game_csv_dir = game_dir, output_path = out)
  res <- readr::read_csv(out, show_col_types = FALSE)
  expect_true(all(as.Date(res$Date) == as.Date("2026-06-05")))
})
```
- [ ] **Step 2: Run, expect FAIL** — `Rscript -e 'testthat::test_file("tests/testthat/test-sync.R")'` (internal date is 2025-10-01, so assertion fails).
- [ ] **Step 3: Implement** — in `build_combined_csv`, read files individually and override Date:
```r
read_one <- function(path) {
  d <- readr::read_csv(path, show_col_types = FALSE)
  m <- regmatches(basename(path), regexpr("^\\d{8}", basename(path)))
  if (length(m) == 1) {
    d$Date <- as.Date(m, "%Y%m%d")
  } else {
    message("build_combined_csv: no date prefix on '", basename(path), "', keeping internal Date")
  }
  d
}
combined <- dplyr::bind_rows(lapply(csv_files, read_one))
```
(Replaces the existing `bind_rows(lapply(csv_files, readr::read_csv, ...))` line.)
- [ ] **Step 4: Run, expect PASS** (both this and the existing combine tests).
- [ ] **Step 5: Commit** — `git commit -m "Use filename date as game date in build_combined_csv"`

---

### Task 2: `schedule.R` — parse + fetch

**Files:**
- Create: `schedule.R`, `tests/testthat/fixtures/schedule.html`, `tests/testthat/test-schedule.R`

**Interfaces:**
- Produces:
  - `SCHEDULE_URL` (chr constant)
  - `parse_schedule(doc)` — takes an `xml_document` (rvest `read_html` result), returns a tibble `date(Date), opponent(chr), home_away(chr), result(chr|NA), played(lgl)`.
  - `fetch_schedule(url = SCHEDULE_URL)` — `read_html` + `parse_schedule`, `tryCatch` → `NULL` on error.

- [ ] **Step 1: Save a real fixture** — `Rscript -e 'download.file(schedule_url, "tests/testthat/fixtures/schedule.html")'` equivalent; capture the live HTML once into the fixture file.
- [ ] **Step 2: Failing test** — `tests/testthat/test-schedule.R`:
```r
library(testthat); library(rvest)
.root <- dirname(dirname(getwd())); source(file.path(.root, "schedule.R"), local = TRUE)
test_that("parse_schedule extracts games with played flag", {
  doc <- read_html(file.path(.root, "tests/testthat/fixtures/schedule.html"))
  s <- parse_schedule(doc)
  expect_true(all(c("date","opponent","home_away","result","played") %in% names(s)))
  expect_gte(nrow(s), 30)
  expect_true(any(s$opponent == "Sonoma Stompers"))
  expect_true(any(s$played)); expect_true(any(!s$played))
  expect_true(any(s$date == as.Date("2026-06-05") & s$home_away == "Home"))
})
```
- [ ] **Step 3: Run, expect FAIL** (`parse_schedule` undefined).
- [ ] **Step 4: Implement `schedule.R`** — derive the CSS/structure selectors from the fixture (inspect with `html_elements`), build the tibble; `played <- !is.na(result) & !grepl("not yet played", result, TRUE)`; `fetch_schedule` wraps `read_html(url)` + `parse_schedule` in `tryCatch(..., error=function(e){message("schedule fetch failed: ", conditionMessage(e)); NULL})`.
- [ ] **Step 5: Run, expect PASS.**
- [ ] **Step 6: Commit** — `git commit -m "Add schedule.R: fetch and parse the season schedule"`

---

### Task 3: `build_coverage()` — join schedule to CSV files

**Files:**
- Modify: `schedule.R`
- Test: `tests/testthat/test-schedule.R`

**Interfaces:**
- Consumes: a schedule tibble (Task 2), a character vector of CSV file paths.
- Produces: `build_coverage(schedule, csv_files)` → `list(games = tibble(<schedule cols> + data_status, n_csv, pitches), orphans = chr)`. `data_status ∈ {"tracked","No CSV","upcoming"}`. `pitches` may be `NA` (coverage by filename date only; pitch count filled by caller if data available — see note).

- [ ] **Step 1: Failing test**:
```r
test_that("build_coverage flags played games without CSV and orphan files", {
  sched <- tibble::tibble(
    date = as.Date(c("2026-06-05","2026-06-07","2026-07-08")),
    opponent = c("Alameda Merchants","Walnut Creek Crawdads","Sonoma Stompers"),
    home_away = c("Home","Away","Home"), result = c("W, 5-4","L, 9-4", NA),
    played = c(TRUE, TRUE, FALSE))
  csvs <- c("20260605-SanBrunoPark-1_unverified.csv", "20260622-SanBrunoPark-1_unverified.csv")
  cov <- build_coverage(sched, csvs)
  expect_equal(cov$games$data_status[cov$games$date == as.Date("2026-06-05")], "tracked")
  expect_equal(cov$games$data_status[cov$games$date == as.Date("2026-06-07")], "No CSV")
  expect_equal(cov$games$data_status[cov$games$date == as.Date("2026-07-08")], "upcoming")
  expect_true(any(grepl("20260622", cov$orphans)))
})
```
- [ ] **Step 2: Run, expect FAIL.**
- [ ] **Step 3: Implement `build_coverage`** in `schedule.R`:
```r
build_coverage <- function(schedule, csv_files) {
  csv_dates <- as.Date(regmatches(basename(csv_files), regexpr("^\\d{8}", basename(csv_files))), "%Y%m%d")
  have <- table(csv_dates)
  status <- ifelse(!schedule$played, "upcoming",
             ifelse(schedule$date %in% csv_dates, "tracked", "No CSV"))
  games <- dplyr::mutate(schedule, data_status = status,
             n_csv = as.integer(have[as.character(schedule$date)]) )
  games$n_csv[is.na(games$n_csv)] <- 0L
  orphans <- basename(csv_files)[!csv_dates %in% schedule$date]
  list(games = games, orphans = sort(unique(orphans)))
}
```
- [ ] **Step 4: Run, expect PASS.**
- [ ] **Step 5: Commit** — `git commit -m "Add build_coverage: match schedule to available CSV files"`

---

### Task 4: Wire schedule into the app + coach "Schedule" tab

**Files:**
- Modify: `global.R` (source schedule.R; startup fetch), `server.R` (reactives + tab outputs), `global.R` coach layout (`tabPanel`).

**Interfaces:**
- Consumes: `fetch_schedule`, `build_coverage`, `GAME_CSV_DIR`.
- Produces: `output$coach_schedule_table`, reactive `coverage()`.

- [ ] **Step 1:** In `global.R`, after `source("sync_drive.R")`, add `source("schedule.R")`.
- [ ] **Step 2:** In `server.R`, add reactives:
```r
schedule_rv <- reactiveVal(tryCatch(fetch_schedule(), error = function(e) NULL))
coverage <- reactive({
  s <- schedule_rv(); if (is.null(s)) return(NULL)
  build_coverage(s, list.files(GAME_CSV_DIR, pattern="\\.csv$", full.names = TRUE))
})
```
Refresh `schedule_rv(fetch_schedule())` inside the existing Drive-sync button observer.
- [ ] **Step 3:** Add `tabPanel("Schedule", uiOutput("coach_schedule_ui"))` to the coach `main_tabs` tabsetPanel in `global.R`.
- [ ] **Step 4:** Implement `output$coach_schedule_ui` in `server.R`: if `coverage()` NULL → "Schedule unavailable" note; else a summary line ("Tracked X of Y played games") + a `DT::datatable` of `games` (Date, Opponent, H/A, Result, Data) with "No CSV" rows styled red, and an "Unmatched CSV files" block if `orphans` non-empty.
- [ ] **Step 5: Manual check** — run app, log in as coach, open Schedule tab; confirm Jun 3/7/9/16/17 show "No CSV", tracked games show counts, orphans listed.
- [ ] **Step 6: Commit** — `git commit -m "Add coach Schedule tab with CSV coverage"`

---

### Task 5: Player-account missing-CSV note

**Files:**
- Modify: `server.R` (`player_ui` or `player_content` area)

**Interfaces:**
- Consumes: `coverage()`.
- Produces: `output$player_missing_csv_note`.

- [ ] **Step 1:** Add `output$player_missing_csv_note <- renderUI({...})`: if `coverage()` NULL → `NULL`; else filter `games` to `data_status == "No CSV"`, and render a compact note: "Missing game data (no CSV yet): Jun 3 vs Menlo Park, …" (empty → `NULL`).
- [ ] **Step 2:** Place `uiOutput("player_missing_csv_note")` in the player content area (top of `player_content`, above the section).
- [ ] **Step 3: Manual check** — log in as a player; confirm the note lists the missing games (or is absent if none/unavailable).
- [ ] **Step 4: Commit** — `git commit -m "Show missing-CSV games in player accounts"`

---

### Task 6: Full verification + deploy

- [ ] **Step 1:** Run full test suite — `Rscript -e 'testthat::test_dir("tests/testthat")'`; all pass.
- [ ] **Step 2:** Boot app locally; verify coach Schedule tab + player note render with no errors in the log.
- [ ] **Step 3:** Deploy — `Rscript deploy_app.R` (network on); confirm success URL.
- [ ] **Step 4:** Verify live: load site, coach Schedule tab shows coverage, a player sees the note.
- [ ] **Step 5: Commit/push** any remaining changes.

## Self-Review

- Spec coverage: filename dates (T1), fetch/parse (T2), coverage match incl. orphans (T3), coach tab (T4), player note (T5), graceful fallback (T2/T4/T5), deploy/rvest (T6). ✓
- Types consistent: `parse_schedule`→tibble cols reused by `build_coverage`; `coverage()` shape consumed by T4/T5. ✓
- Pitch-count note: coverage is by filename date; the coach table's "Data" shows `n_csv` file count (and tracked/No CSV), not per-pitch totals, to avoid loading full data in the schedule reactive. Acceptable per spec intent (knowing coverage).
