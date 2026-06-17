library(testthat)

.proj_root <- dirname(dirname(getwd()))
.orig_wd   <- getwd()
setwd(.proj_root)
source(file.path(.proj_root, "global.R"), local = TRUE)
setwd(.orig_wd)

test_that("ring_df produces a closed circle at the correct radius", {
  df <- ring_df(12)
  expect_named(df, c("x", "y", "r"))
  expect_gte(nrow(df), 200)
  radii <- sqrt(df$x^2 + df$y^2)
  expect_true(all(abs(radii - 12) < 0.01))
  expect_equal(unique(df$r), 12)
})

test_that("ring_df(0) produces points at origin", {
  df <- ring_df(0)
  radii <- sqrt(df$x^2 + df$y^2)
  expect_true(all(abs(radii) < 0.01))
})
