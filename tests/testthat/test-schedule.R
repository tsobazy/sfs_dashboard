library(testthat)
library(rvest)
library(dplyr)

.root <- dirname(dirname(getwd()))
source(file.path(.root, "schedule.R"), local = TRUE)

fixture <- file.path(.root, "tests/testthat/fixtures/schedule.html")

test_that("parse_schedule extracts games with dates, home/away, and played flag", {
  s <- parse_schedule(read_html(fixture))
  expect_true(all(c("date", "opponent", "home_away", "result", "played") %in% names(s)))
  expect_gte(nrow(s), 35)

  # played games are dated from data-boxscore (reliable)
  expect_equal(sum(s$played), 18L)
  jun5 <- filter(s, date == as.Date("2026-06-05"))
  expect_equal(nrow(jun5), 1L)
  expect_equal(jun5$opponent, "Alameda Merchants")
  expect_equal(jun5$home_away, "Home")
  expect_true(jun5$played)

  jun24 <- filter(s, date == as.Date("2026-06-24"), played)
  expect_equal(jun24$home_away, "Away")

  # at least one upcoming game, and inferred July dates land correctly
  expect_true(any(!s$played))
  expect_true(any(s$date == as.Date("2026-07-08") & s$home_away == "Home"))
})

test_that("build_coverage flags played games without CSV, tracks present ones, lists orphans", {
  sched <- tibble::tibble(
    date      = as.Date(c("2026-06-05", "2026-06-07", "2026-07-08")),
    opponent  = c("Alameda Merchants", "Walnut Creek Crawdads", "Sonoma Stompers"),
    home_away = c("Home", "Away", "Home"),
    result    = c("W, 5-4", "L, 9-4", NA),
    played    = c(TRUE, TRUE, FALSE)
  )
  csvs <- c("20260605-SanBrunoPark-1_unverified.csv",
            "20260622-SanBrunoPark-1_unverified.csv")

  cov <- build_coverage(sched, csvs)
  g <- cov$games
  expect_equal(g$data_status[g$date == as.Date("2026-06-05")], "tracked")
  expect_equal(g$n_csv[g$date == as.Date("2026-06-05")], 1L)
  expect_equal(g$data_status[g$date == as.Date("2026-06-07")], "No CSV")
  expect_equal(g$data_status[g$date == as.Date("2026-07-08")], "upcoming")
  expect_true(any(grepl("20260622", cov$orphans)))
  expect_false(any(grepl("20260605", cov$orphans)))
})
