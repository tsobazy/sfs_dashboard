# roster.R — credentials and position lookup for SF Seagulls 2026

credentials <- data.frame(
  user = c(
    # Staff
    "coach", "analytics",
    # Players
    "bryce_brooks", "sebastian_ultreras", "declan_mendel", "emilio_feliciano",
    "davis_germann", "theodore_tsouras", "benjamin_joost", "finn_whalen",
    "matthew_potter", "louden_hilliard", "caid_heflin", "jacob_gilbreath",
    "joseph_steidel", "blake_cowans", "jake_brewer", "ethan_lopez",
    "caleb_garrison", "marcus_graham", "connor_wood", "taylor_easthope",
    "derek_waldvogel", "tanner_wall", "armando_hurtado", "brandon_swanson",
    "christian_lamothe", "jb_ferreira", "kai_hanasaki", "branson_derrington",
    "camren_boyd", "luka_shah", "alan_ramirez"
  ),
  password = c(
    # Staff
    "2026", "2026",
    # Players (default = jersey number as string)
    "1", "3", "6", "7", "9", "12", "13", "14", "16", "17", "20", "22",
    "25", "26", "29", "30", "31", "32", "34", "35", "36", "37", "38",
    "39", "40", "43", "50", "53", "54", "55", "56"
  ),
  role = c(
    rep("coach", 2),
    rep("player", 31)
  ),
  display_name = c(
    "Coach", "Analytics",
    "Bryce Brooks", "Sebastian Ultreras", "Declan Mendel", "Emilio Feliciano",
    "Davis Germann", "Theodore Tsouras", "Benjamin Joost", "Finn Whalen",
    "Matthew Potter", "Louden Hilliard", "Caid Heflin", "Jacob Gilbreath",
    "Joseph Steidel", "Blake Cowans", "Jake Brewer", "Ethan Lopez",
    "Caleb Garrison", "Marcus Graham", "Connor Wood", "Taylor Easthope",
    "Derek Waldvogel", "Tanner Wall", "Armando Hurtado", "Brandon Swanson",
    "Christian LaMothe", "JB Ferreira", "Kai Hanasaki", "Branson Derrington",
    "Camren Boyd", "Luka Shah", "Alan Ramirez"
  ),
  player_name = c(
    rep(NA_character_, 2),
    "Bryce Brooks", "Sebastian Ultreras", "Declan Mendel", "Emilio Feliciano",
    "Davis Germann", "Theodore Tsouras", "Benjamin Joost", "Finn Whalen",
    "Matthew Potter", "Louden Hilliard", "Caid Heflin", "Jacob Gilbreath",
    "Joseph Steidel", "Blake Cowans", "Jake Brewer", "Ethan Lopez",
    "Caleb Garrison", "Marcus Graham", "Connor Wood", "Taylor Easthope",
    "Derek Waldvogel", "Tanner Wall", "Armando Hurtado", "Brandon Swanson",
    "Christian LaMothe", "JB Ferreira", "Kai Hanasaki", "Branson Derrington",
    "Camren Boyd", "Luka Shah", "Alan Ramirez"
  ),
  player_type = c(
    rep(NA_character_, 2),
    "hitter", "hitter", "pitcher", "hitter", "hitter",
    "pitcher", "pitcher", "pitcher", "pitcher", "pitcher",
    "hitter", "hitter", "pitcher", "hitter", "hitter",
    "hitter", "pitcher", "hitter", "pitcher", "pitcher",
    "hitter", "hitter", "hitter", "hitter", "hitter",
    "pitcher", "pitcher", "pitcher", "pitcher", "pitcher",
    "hitter"
  ),
  jersey = c(
    rep(NA_integer_, 2),
    1L, 3L, 6L, 7L, 9L, 12L, 13L, 14L, 16L, 17L, 20L, 22L,
    25L, 26L, 29L, 30L, 31L, 32L, 34L, 35L, 36L, 37L, 38L,
    39L, 40L, 43L, 50L, 53L, 54L, 55L, 56L
  ),
  stringsAsFactors = FALSE
)

roster_positions <- data.frame(
  player_name = c(
    "Bryce Brooks", "Sebastian Ultreras", "Declan Mendel", "Emilio Feliciano",
    "Davis Germann", "Theodore Tsouras", "Benjamin Joost", "Finn Whalen",
    "Matthew Potter", "Louden Hilliard", "Caid Heflin", "Jacob Gilbreath",
    "Joseph Steidel", "Blake Cowans", "Jake Brewer", "Ethan Lopez",
    "Caleb Garrison", "Marcus Graham", "Connor Wood", "Taylor Easthope",
    "Derek Waldvogel", "Tanner Wall", "Armando Hurtado", "Brandon Swanson",
    "Christian LaMothe", "JB Ferreira", "Kai Hanasaki", "Branson Derrington",
    "Camren Boyd", "Luka Shah", "Alan Ramirez"
  ),
  position = c(
    "INF", "INF", "RHP", "INF", "OF",
    "LHP", "RHP", "RHP", "RHP", "RHP",
    "OF", "OF", "LHP", "C", "INF",
    "C", "RHP", "C/OF", "RHP", "LHP",
    "1B/OF", "INF/OF", "C/OF", "INF", "INF",
    "RHP", "RHP", "RHP", "LHP", "RHP",
    "INF"
  ),
  jersey = c(
    1L, 3L, 6L, 7L, 9L, 12L, 13L, 14L, 16L, 17L, 20L, 22L,
    25L, 26L, 29L, 30L, 31L, 32L, 34L, 35L, 36L, 37L, 38L,
    39L, 40L, 43L, 50L, 53L, 54L, 55L, 56L
  ),
  stringsAsFactors = FALSE
)
