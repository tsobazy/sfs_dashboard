library(testthat)

resolve_range <- function(sel, all_dates) {
  switch(sel,
    "Season" = NULL,
    "Last 5" = head(sort(all_dates, decreasing = TRUE), 5),
    as.Date(sel)
  )
}

test_that("Season returns NULL", {
  dates <- as.Date(c("2025-11-22", "2025-10-25"))
  expect_null(resolve_range("Season", dates))
})

test_that("Last 5 returns 5 most recent dates", {
  all_dates <- as.Date(c("2025-11-22","2025-11-15","2025-11-08",
                          "2025-11-01","2025-10-25","2025-10-18","2025-10-11"))
  result <- resolve_range("Last 5", all_dates)
  expect_length(result, 5)
  expect_equal(result[1], as.Date("2025-11-22"))
  expect_equal(result[5], as.Date("2025-10-25"))
})

test_that("specific date string parses to Date", {
  dates <- as.Date(c("2025-11-22", "2025-10-25"))
  result <- resolve_range("2025-11-22", dates)
  expect_equal(result, as.Date("2025-11-22"))
})
