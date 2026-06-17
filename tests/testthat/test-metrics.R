library(testthat)

# Source global to get metric functions and constants
# test_dir changes cwd to tests/testthat; go up two levels to project root
.proj_root <- dirname(dirname(getwd()))
.orig_wd <- getwd()
setwd(.proj_root)
source(file.path(.proj_root, "global.R"), local = TRUE)
setwd(.orig_wd)

test_that("strike_pct counts strike outcomes correctly", {
  calls <- c("StrikeCalled", "BallCalled", "InPlay", "StrikeSwinging",
             "FoulBallNotFieldable", "BallCalled")
  expect_equal(strike_pct(calls), 4/6)
})

test_that("strike_pct returns 0 for all balls", {
  expect_equal(strike_pct(c("BallCalled", "BallCalled")), 0)
})

test_that("whiff_pct returns swinging-strike rate among swings", {
  calls <- c("StrikeSwinging", "InPlay", "FoulBallNotFieldable", "BallCalled")
  # swings = StrikeSwinging + InPlay + FoulBallNotFieldable = 3
  # whiffs = StrikeSwinging = 1
  expect_equal(whiff_pct(calls), 1/3)
})

test_that("whiff_pct returns NA when no swings", {
  expect_true(is.na(whiff_pct(c("BallCalled", "BallCalled"))))
})

test_that("csw_pct counts called + swinging strikes", {
  calls <- c("StrikeCalled", "StrikeSwinging", "BallCalled", "InPlay")
  expect_equal(csw_pct(calls), 2/4)
})

test_that("chase_pct returns swing rate on out-of-zone pitches", {
  side   <- c(1.5, 0.0, 0.0, 0.0)    # 1st is OOZ (side > SZ_RIGHT)
  height <- c(2.5, 2.5, 2.5, 0.5)    # 4th is OOZ (below SZ_BOT)
  calls  <- c("StrikeSwinging", "BallCalled", "BallCalled", "InPlay")
  # OOZ pitches: index 1 (side=1.5 > SZ_RIGHT=0.83) and index 4 (height=0.5 < SZ_BOT=1.50)
  # Swings on OOZ: index 1 (StrikeSwinging), index 4 (InPlay) = 2 swings / 2 OOZ pitches
  expect_equal(chase_pct(side, height, calls), 1.0)
})

test_that("chase_pct returns NA when no OOZ pitches", {
  side   <- c(0.0, 0.0)
  height <- c(2.5, 2.5)
  calls  <- c("StrikeSwinging", "BallCalled")
  expect_true(is.na(chase_pct(side, height, calls)))
})

test_that("hard_hit_pct calculates correctly", {
  ev <- c(97, 85, 100, 90, 95)
  # >= 95: indices 1, 3, 5 = 3 of 5
  expect_equal(hard_hit_pct(ev), 3/5)
})

test_that("hard_hit_pct ignores NAs", {
  ev <- c(97, NA, 85, 95)
  expect_equal(hard_hit_pct(ev), 2/3)
})

test_that("barrel_pct identifies barrels correctly", {
  ev <- c(100, 85, 99, 100)
  la <- c(28,  28,  10,  28)
  # Barrel: ev >= 98 AND la >= 26 AND la <= 30
  # Index 1: 100 >= 98, 28 in [26,30] → barrel
  # Index 3: 99 >= 98, 10 not in [26,30] → not barrel
  # Index 4: 100 >= 98, 28 in [26,30] → barrel
  expect_equal(barrel_pct(ev, la), 2/4)
})

test_that("gb_pct calculates ground ball rate among BIP", {
  ht <- c("GroundBall", "FlyBall", "LineDrive", "GroundBall", "Popup")
  expect_equal(gb_pct(ht), 2/5)
})

test_that("gb_pct returns NA when no BIP", {
  expect_true(is.na(gb_pct(c("Undefined", "Undefined"))))
})
