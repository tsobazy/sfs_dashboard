# setup_player_photos.R
# Run once from the project root: Rscript setup_player_photos.R
# Downloads headshots into www/players/. Players with NA photo_url get no file.

library(tibble)

roster_photos <- tribble(
  ~player_name,          ~photo_url,
  "Bryce Brooks",        "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_bryce_brooks.jpg",
  "Sebastian Ultreras",  "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_sebastian_ultreras.jpg",
  "Declan Mendel",       "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_declan_mendel.jpg",
  "Emilio Feliciano",    "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_emilio_feliciano.jpg",
  "Davis Germann",       "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_davis_germann.jpg",
  "Theodore Tsouras",    "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_theodore_tsouras.jpg",
  "Benjamin Joost",      "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_benjamin_joost.jpg",
  "Finn Whalen",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_finn_whalen.jpg",
  "Matthew Potter",      "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_matthew_potter.jpg",
  "Louden Hilliard",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_louden_hilliard.jpg",
  "Caid Heflin",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_caid_heflin.jpg",
  "Jacob Gilbreath",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_jacob_gilbreath.jpg",
  "Joseph Steidel",      "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_joseph_steidel.jpg",
  "Blake Cowans",        "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_blake_cowans.jpg",
  "Jake Brewer",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_jake_brewer.jpg",
  "Ethan Lopez",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_ethan_lopez.jpg",
  "Caleb Garrison",      "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_caleb_garrison.jpg",
  "Marcus Graham",       "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_marcus_graham.jpg",
  "Connor Wood",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_connor_wood.jpg",
  "Taylor Easthope",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_taylor_easthope.jpg",
  "Derek Waldvogel",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_derek_waldvogel.jpg",
  "Tanner Wall",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_tanner_wall.jpg",
  "Armando Hurtado",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_armando_hurtado.jpg",
  "Brandon Swanson",     "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_brandon_swanson.jpg",
  "Christian LaMothe",   "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_christian_lamothe.jpg",
  "JB Ferreira",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_jb_ferreira.jpg",
  "Branson Derrington",  "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_branson_derrington.jpg",
  "Camren Boyd",         "https://collegeseagulls.com/sports/bsb/2026/photos/0001/hs_camren_boyd.jpg",
  "Luka Shah",           NA_character_,
  "Alan Ramirez",        NA_character_
)

if (!dir.exists("www/players")) dir.create("www/players", recursive = TRUE)

slugify <- function(name) gsub("[^a-z0-9]+", "_", tolower(trimws(name)))

for (i in seq_len(nrow(roster_photos))) {
  row <- roster_photos[i, ]
  if (is.na(row$photo_url)) next
  dest <- file.path("www/players", paste0(slugify(row$player_name), ".jpg"))
  cmd <- sprintf(
    "curl -sL -A 'Mozilla/5.0' -o '%s' '%s'",
    dest, row$photo_url
  )
  exit_code <- system(cmd)
  if (exit_code != 0) {
    message("Failed (exit ", exit_code, ") for ", row$player_name)
  } else {
    message("OK: ", row$player_name)
  }
}

message("Done. Players without a downloaded photo will show no image in the app.")
