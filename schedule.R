# schedule.R — fetch and parse the official 2026 season schedule, and match it
# against the game CSVs we actually have so the app can flag missing data.
#
# The schedule page (Sidearm Sports) renders games as `.event-row` elements.
# Played games carry a reliable full date in their `data-boxscore` attribute
# (".../20260605_...xml"); upcoming games only show a day number, so their date
# is inferred chronologically. Home/away comes from the `.event-location-badge`
# text ("vs" = home, "at" = away).

SCHEDULE_URL <- "https://collegeseagulls.com/sports/bsb/2026/schedule"

# Parse an rvest-read schedule document into a tibble of games.
# Returns columns: date (Date), opponent (chr), home_away (chr),
#                  result (chr|NA), played (lgl).
parse_schedule <- function(doc) {
  rows <- rvest::html_elements(doc, ".event-row")

  recs <- lapply(rows, function(r) {
    txt <- rvest::html_text2(r)
    list(
      badge   = rvest::html_text2(rvest::html_element(r, ".event-location-badge")),
      opp     = rvest::html_text2(rvest::html_element(r, ".event-opponent-name")),
      res     = rvest::html_text2(rvest::html_element(r, ".event-result")),
      box     = rvest::html_attr(r, "data-boxscore"),
      daytx   = rvest::html_text2(rvest::html_element(r, ".date")),
      is_next = grepl("Next Event", txt)
    )
  })
  # Drop the highlighted "Next Event" duplicate row.
  recs <- Filter(function(x) !isTRUE(x$is_next), recs)

  date_re <- "[0-9]{8}"
  digits_re <- "[^0-9]"

  dates   <- as.Date(rep(NA_real_, length(recs)), origin = "1970-01-01")
  has_box <- logical(length(recs))
  last    <- as.Date(NA)
  for (i in seq_along(recs)) {
    b <- recs[[i]]$box
    if (!is.na(b) && grepl(date_re, b)) {
      has_box[i] <- TRUE
      d <- as.Date(regmatches(b, regexpr(date_re, b)), "%Y%m%d")
    } else if (grepl("Today", recs[[i]]$daytx %||% "", ignore.case = TRUE)) {
      d <- Sys.Date()   # schedule is fetched live, so "Today" is the current date
    } else {
      dn <- suppressWarnings(as.integer(gsub(digits_re, "", recs[[i]]$daytx)))
      if (is.na(dn) || is.na(last)) {
        d <- as.Date(NA)
      } else {
        mo <- as.integer(format(last, "%m")); yr <- as.integer(format(last, "%Y"))
        if (dn < as.integer(format(last, "%d"))) {
          mo <- mo + 1L; if (mo > 12L) { mo <- 1L; yr <- yr + 1L }
        }
        d <- as.Date(sprintf("%04d-%02d-%02d", yr, mo, dn))
      }
    }
    dates[i] <- d
    if (!is.na(d)) last <- d
  }

  opp       <- vapply(recs, function(x) x$opp %||% NA_character_, character(1))
  badge     <- vapply(recs, function(x) x$badge %||% "", character(1))
  res_raw   <- vapply(recs, function(x) x$res %||% "", character(1))
  home_away <- ifelse(grepl("^vs", badge), "Home",
                ifelse(grepl("^at", badge), "Away", NA_character_))
  result    <- ifelse(nzchar(res_raw) & grepl("^[WLT],", res_raw), res_raw, NA_character_)
  # A game is "played" once it has a boxscore — a reliable signal that survives
  # even when the result text doesn't come through (some hosts strip it).
  played    <- has_box

  out <- tibble::tibble(
    date = dates, opponent = opp, home_away = home_away,
    result = result, played = played
  )
  # The page repeats the next/today game as a highlight card; drop exact dups.
  dplyr::distinct(out)
}

# Fetch + parse the live schedule. Returns NULL on any failure so callers can
# show "schedule unavailable" without the app crashing. Sends a browser
# User-Agent so the host serves the full page (some strip content for default
# clients), falling back to a plain read_html if that path errors.
SCHEDULE_UA <- paste0(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 ",
  "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")

fetch_schedule <- function(url = SCHEDULE_URL) {
  tryCatch({
    doc <- tryCatch({
      resp <- httr2::req_perform(httr2::req_user_agent(httr2::request(url), SCHEDULE_UA))
      rvest::read_html(httr2::resp_body_string(resp))
    }, error = function(e) rvest::read_html(url))
    parse_schedule(doc)
  }, error = function(e) {
    message("schedule fetch failed: ", conditionMessage(e)); NULL
  })
}

# Local NULL-coalescing helper, only defined when shiny's `%||%` isn't already
# present (tests source schedule.R without shiny). Never overrides shiny's.
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

# Match a schedule tibble against the CSV files present. Returns
#   list(games = <schedule + data_status, n_csv>, orphans = <chr>)
# data_status in {"tracked","No CSV","upcoming"}; orphans are CSV filenames
# whose date matches no scheduled game.
build_coverage <- function(schedule, csv_files) {
  base <- basename(csv_files)
  pre  <- regmatches(base, regexpr("^[0-9]{8}", base))
  # keep only files that actually have a date prefix
  has_pre  <- grepl("^[0-9]{8}", base)
  csv_dates <- as.Date(rep(NA_real_, length(base)), origin = "1970-01-01")
  csv_dates[has_pre] <- as.Date(pre, "%Y%m%d")

  counts <- table(csv_dates[!is.na(csv_dates)])
  status <- ifelse(!schedule$played, "upcoming",
             ifelse(schedule$date %in% csv_dates, "tracked", "No CSV"))
  n_csv <- as.integer(counts[as.character(schedule$date)])
  n_csv[is.na(n_csv)] <- 0L

  games <- schedule
  games$data_status <- status
  games$n_csv <- n_csv

  orphans <- sort(unique(base[has_pre & !(csv_dates %in% schedule$date)]))
  list(games = games, orphans = orphans)
}
