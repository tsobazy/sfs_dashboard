library(testthat)

.proj_root <- dirname(dirname(getwd()))
.orig_wd   <- getwd()
setwd(.proj_root)
source(file.path(.proj_root, "global.R"), local = TRUE)
setwd(.orig_wd)

test_that("spray_xy converts 0 degrees (straight center) to y-axis", {
  pt <- spray_xy(0, 300)
  expect_equal(pt$x, 0, tolerance = 0.01)
  expect_equal(pt$y, 300, tolerance = 0.01)
})

test_that("spray_xy converts 45 degrees to equal x and y", {
  pt <- spray_xy(45, 100)
  expect_equal(pt$x, pt$y, tolerance = 0.01)
})

test_that("spray_xy converts -45 degrees to negative x", {
  pt <- spray_xy(-45, 100)
  expect_true(pt$x < 0)
  expect_true(pt$y > 0)
})

test_that("field_outline_df returns data frame with x and y", {
  df <- field_outline_df()
  expect_true(is.data.frame(df))
  expect_true("x" %in% names(df))
  expect_true("y" %in% names(df))
  expect_gt(nrow(df), 10)
})
