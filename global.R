library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(shinymanager)
library(scales)

# ── Data ──────────────────────────────────────────────────────────────────────
data <- read_csv("all_fall_25.csv", show_col_types = FALSE)

data$TaggedPitchType <- if_else(
  data$TaggedPitchType %in% c("Other", NA_character_), "Undefined", data$TaggedPitchType
)
data$Count <- paste0(data$Balls, "-", data$Strikes)

# ── Constants ─────────────────────────────────────────────────────────────────
SZ_LEFT  <- -0.83
SZ_RIGHT <-  0.83
SZ_BOT   <-  1.50
SZ_TOP   <-  3.50

PITCH_COLORS <- c(
  FourSeamFastBall = "#E63946", Sinker          = "#F4A261",
  ChangeUp         = "#2A9D8F", Curveball       = "#457B9D",
  Slider           = "#6A4C93", Sweeper         = "#9B2226",
  Cutter           = "#E9C46A", Splitter        = "#264653",
  Fastball         = "#E76F51", TwoSeamFastBall = "#F4D35E",
  Undefined        = "#AAAAAA"
)

BIP_TYPES <- c("GroundBall", "FlyBall", "LineDrive", "Popup")

# ── Metric Functions ──────────────────────────────────────────────────────────
strike_pct <- function(calls) {
  mean(calls %in% c("StrikeCalled", "StrikeSwinging",
                    "FoulBallNotFieldable", "FoulBallFieldable", "InPlay"),
       na.rm = TRUE)
}

whiff_pct <- function(calls) {
  swings <- calls %in% c("StrikeSwinging", "FoulBallNotFieldable",
                          "FoulBallFieldable", "InPlay")
  n_swings <- sum(swings, na.rm = TRUE)
  if (n_swings == 0) return(NA_real_)
  sum(calls == "StrikeSwinging", na.rm = TRUE) / n_swings
}

csw_pct <- function(calls) {
  mean(calls %in% c("StrikeCalled", "StrikeSwinging"), na.rm = TRUE)
}

chase_pct <- function(side, height, calls) {
  ooz <- side < SZ_LEFT | side > SZ_RIGHT | height < SZ_BOT | height > SZ_TOP
  swings <- calls %in% c("StrikeSwinging", "FoulBallNotFieldable",
                          "FoulBallFieldable", "InPlay")
  n_ooz <- sum(ooz, na.rm = TRUE)
  if (n_ooz == 0) return(NA_real_)
  sum(ooz & swings, na.rm = TRUE) / n_ooz
}

hard_hit_pct <- function(ev) {
  ev <- ev[!is.na(ev)]
  if (length(ev) == 0) return(NA_real_)
  mean(ev >= 95)
}

barrel_pct <- function(ev, la) {
  ok <- !is.na(ev) & !is.na(la)
  if (sum(ok) == 0) return(NA_real_)
  mean(ev[ok] >= 98 & la[ok] >= 26 & la[ok] <= 30)
}

gb_pct <- function(ht) {
  bip <- ht[ht %in% BIP_TYPES]
  if (length(bip) == 0) return(NA_real_)
  sum(bip == "GroundBall") / length(bip)
}

# ── Theme ─────────────────────────────────────────────────────────────────────
theme_seagulls <- function() {
  theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    panel.grid.major  = element_line(color = "#e8e8e8"),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(face = "bold", size = 14, color = "#0a1628"),
    plot.subtitle     = element_text(size = 11, color = "#555555"),
    axis.text         = element_text(color = "#333333"),
    legend.background = element_rect(fill = "white"),
    strip.background  = element_rect(fill = "#f0f0f0")
  )
}

plotly_white <- function(p) {
  p %>% layout(
    paper_bgcolor = "white",
    plot_bgcolor  = "white",
    font          = list(color = "#0a1628")
  )
}

source("roster.R")
