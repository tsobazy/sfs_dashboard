library(testthat)

.proj_root <- dirname(dirname(getwd()))
.orig_wd   <- getwd()
setwd(.proj_root)
source(file.path(.proj_root, "global.R"), local = TRUE)
setwd(.orig_wd)

test_that("PITCH_COLORS uses Statcast hex values", {
  expect_equal(unname(PITCH_COLORS["FourSeamFastBall"]), "#D22D49")
  expect_equal(unname(PITCH_COLORS["Curveball"]),        "#00D1ED")
  expect_equal(unname(PITCH_COLORS["ChangeUp"]),         "#1DBE3A")
  expect_equal(unname(PITCH_COLORS["Slider"]),           "#EEE716")
  expect_equal(unname(PITCH_COLORS["Undefined"]),        "#AAAAAA")
})

test_that("PITCH_CATEGORY_COLORS has correct keys and Statcast-family hex", {
  expect_true("Fastball"      %in% names(PITCH_CATEGORY_COLORS))
  expect_true("Breaking Ball" %in% names(PITCH_CATEGORY_COLORS))
  expect_true("Offspeed"      %in% names(PITCH_CATEGORY_COLORS))
  expect_equal(unname(PITCH_CATEGORY_COLORS["Fastball"]),      "#D22D49")
  expect_equal(unname(PITCH_CATEGORY_COLORS["Breaking Ball"]), "#00D1ED")
  expect_equal(unname(PITCH_CATEGORY_COLORS["Offspeed"]),      "#1DBE3A")
})
