# Generates: seagulls_qr.png (QR for the mobile URL) and
#            seagulls_logins.pdf (printable login handout for coaches + players)
suppressMessages({library(qrcode); library(png)})
source("roster.R")

URL <- "https://tsobazy.shinyapps.io/sfs_dashboard/"

# ── 1) QR code PNG ──────────────────────────────────────────────────────────
qr <- qr_code(URL)
png("seagulls_qr.png", width = 700, height = 700, bg = "white")
par(mar = c(0, 0, 0, 0))
plot(qr)
dev.off()
qrimg <- readPNG("seagulls_qr.png")

navy <- "#1E2A3A"; blue <- "#015294"; grey <- "#64748b"

cred    <- credentials
coaches <- cred[cred$role == "coach", ]
players <- cred[cred$role == "player", ]
players <- players[order(players$jersey), ]

# ── 2) PDF handout ──────────────────────────────────────────────────────────
pdf("seagulls_logins.pdf", width = 8.5, height = 11)

## Page 1 — cover: URL + QR + how-to + coach logins
plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1)); par(mar = c(0,0,0,0))
text(0.5, 0.95, "SF Seagulls Dashboard", col = navy, cex = 2.4, font = 2)
text(0.5, 0.905, "Player & Coach Access", col = grey, cex = 1.3)

text(0.5, 0.845, "Open in any web browser (phone or computer):", col = "black", cex = 1.1)
text(0.5, 0.805, URL, col = blue, cex = 1.15, font = 2)

# QR centred
rasterImage(qrimg, 0.355, 0.50, 0.645, 0.50 + 0.29 * (8.5/11))
text(0.5, 0.485, "Scan to open", col = grey, cex = 1.0)

# How to log in
text(0.08, 0.41, "How to log in", col = navy, cex = 1.25, font = 2, adj = 0)
howto <- c(
  "1.  On any phone or computer, open Safari or Chrome.",
  "2.  Go to the web address shown above.",
  "3.  Enter your username and password from the list.",
  "      -  Username:  firstname_lastname  (e.g. christian_lamothe)",
  "      -  Password:  your jersey number")
for (i in seq_along(howto))
  text(0.08, 0.385 - (i-1)*0.028, howto[i], col = "black", cex = 1.0, adj = 0)

# Coaches box
text(0.08, 0.20, "Coach logins", col = navy, cex = 1.25, font = 2, adj = 0)
text(0.08, 0.165, sprintf("%-16s %-14s %s", "USERNAME", "PASSWORD", "ROLE"),
     col = grey, cex = 1.0, adj = 0, family = "mono")
for (i in seq_len(nrow(coaches)))
  text(0.08, 0.165 - i*0.030,
       sprintf("%-16s %-14s %s", coaches$user[i], coaches$password[i], "Coach"),
       col = "black", cex = 1.05, adj = 0, family = "mono")

## Page 2 — all player logins (two columns)
plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1)); par(mar = c(0,0,0,0))
text(0.5, 0.96, "Player Logins", col = navy, cex = 2.0, font = 2)
text(0.5, 0.925, paste0(nrow(players), " players  -  password = jersey number"),
     col = grey, cex = 1.1)

top <- 0.88; dy <- 0.0235; x0 <- 0.14
hdr <- sprintf("%-22s %-21s %s", "USERNAME", "NAME", "PASSWORD")
text(x0, top, hdr, col = grey, cex = 0.95, adj = 0, family = "mono")
for (i in seq_len(nrow(players)))
  text(x0, top - i*dy,
       sprintf("%-22s %-21s %s", players$user[i],
               players$display_name[i], players$password[i]),
       col = "black", cex = 0.95, adj = 0, family = "mono")
text(0.5, 0.04,
     "Note: Louden Hilliard (17), George Schmitt (18) and Branson Derrington (53) have no game data yet.",
     col = grey, cex = 0.85)

invisible(dev.off())
cat("Wrote seagulls_qr.png and seagulls_logins.pdf\n")
