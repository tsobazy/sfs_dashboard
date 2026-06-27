library(testthat)
library(readr)
library(dplyr)

.proj_root <- dirname(dirname(getwd()))
.orig_wd   <- getwd()
setwd(.proj_root)
source(file.path(.proj_root, "sync_drive.R"), local = TRUE)
setwd(.orig_wd)

# Helper: write a minimal TrackMan-shaped CSV to a temp file
mini_csv <- function(path, pitcher, n = 3) {
  d <- data.frame(
    Date            = as.Date("2025-10-01") + seq_len(n) - 1L,
    Pitcher         = pitcher,
    Batter          = "Smith, John",
    TaggedPitchType = c("FourSeamFastBall", "Other", NA_character_)[seq_len(n) %% 3 + 1],
    PitchCall       = "StrikeCalled",
    Balls           = 0L,
    Strikes         = 0L,
    stringsAsFactors = FALSE
  )
  write_csv(d, path)
  invisible(path)
}

test_that("build_combined_csv combines two CSVs into one file", {
  tmp      <- tempdir()
  game_dir <- file.path(tmp, "games")
  dir.create(game_dir, showWarnings = FALSE)
  out_csv  <- file.path(tmp, "combined.csv")

  mini_csv(file.path(game_dir, "game1.csv"), "Jones, Bob",   n = 3)
  mini_csv(file.path(game_dir, "game2.csv"), "Smith, Alice", n = 3)

  n_rows <- build_combined_csv(game_csv_dir = game_dir, output_path = out_csv)

  expect_equal(n_rows, 6L)
  result <- read_csv(out_csv, show_col_types = FALSE)
  expect_equal(nrow(result), 6L)
  expect_true(all(c("Jones, Bob", "Smith, Alice") %in% result$Pitcher))
})

test_that("build_combined_csv replaces Other/NA TaggedPitchType with Undefined", {
  tmp      <- tempdir()
  game_dir <- file.path(tmp, "games2")
  dir.create(game_dir, showWarnings = FALSE)
  out_csv  <- file.path(tmp, "combined2.csv")

  mini_csv(file.path(game_dir, "game1.csv"), "Jones, Bob", n = 3)

  build_combined_csv(game_csv_dir = game_dir, output_path = out_csv)
  result <- read_csv(out_csv, show_col_types = FALSE)

  expect_false(any(result$TaggedPitchType == "Other",  na.rm = TRUE))
  expect_false(any(is.na(result$TaggedPitchType)))
  expect_true(any(result$TaggedPitchType == "Undefined"))
})

test_that("build_combined_csv adds Count column", {
  tmp      <- tempdir()
  game_dir <- file.path(tmp, "games3")
  dir.create(game_dir, showWarnings = FALSE)
  out_csv  <- file.path(tmp, "combined3.csv")

  mini_csv(file.path(game_dir, "game1.csv"), "Jones, Bob", n = 2)

  build_combined_csv(game_csv_dir = game_dir, output_path = out_csv)
  result <- read_csv(out_csv, show_col_types = FALSE)

  expect_true("Count" %in% names(result))
  expect_equal(result$Count, paste0(result$Balls, "-", result$Strikes))
})

test_that("build_combined_csv errors when directory is empty", {
  tmp      <- tempdir()
  game_dir <- file.path(tmp, "empty_games")
  dir.create(game_dir, showWarnings = FALSE)
  out_csv  <- file.path(tmp, "combined_empty.csv")

  expect_error(
    build_combined_csv(game_csv_dir = game_dir, output_path = out_csv),
    "No CSV files found"
  )
})

test_that("build_combined_csv sets Date from the filename prefix", {
  tmp      <- tempdir()
  game_dir <- file.path(tmp, "fdates")
  dir.create(game_dir, showWarnings = FALSE)
  out_csv  <- file.path(tmp, "fd.csv")

  # mini_csv writes an internal Date of 2025-10-01..; the filename says 2026-06-05,
  # so the combined Date must come from the filename, not the internal column.
  mini_csv(file.path(game_dir, "20260605-SanBrunoPark-1_unverified.csv"), "Jones, Bob", n = 3)

  build_combined_csv(game_csv_dir = game_dir, output_path = out_csv)
  res <- readr::read_csv(out_csv, show_col_types = FALSE)
  expect_true(all(as.Date(res$Date) == as.Date("2026-06-05")))
})

test_that("build_combined_csv keeps internal Date when filename has no date prefix", {
  tmp      <- tempdir()
  game_dir <- file.path(tmp, "nodate")
  dir.create(game_dir, showWarnings = FALSE)
  out_csv  <- file.path(tmp, "nd.csv")

  mini_csv(file.path(game_dir, "game_no_prefix.csv"), "Jones, Bob", n = 3)
  suppressWarnings(suppressMessages(
    build_combined_csv(game_csv_dir = game_dir, output_path = out_csv)
  ))
  res <- readr::read_csv(out_csv, show_col_types = FALSE)
  # internal incrementing dates are preserved when no filename prefix
  expect_equal(as.Date(res$Date), as.Date(c("2025-10-01", "2025-10-02", "2025-10-03")))
})
