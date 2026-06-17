# sync_drive.R — Google Drive sync helpers

GAME_CSV_DIR <- "data/game_csvs"

# Downloads any CSVs in folder_id not already present in local_dir.
# Returns the count of newly downloaded files.
# Requires: googledrive auth already established (run setup_drive_auth.R once).
sync_from_drive <- function(folder_id,
                             local_dir = GAME_CSV_DIR) {
  library(googledrive)
  drive_auth(cache = ".secrets")

  remote_files <- drive_ls(as_id(folder_id), type = "csv")
  if (nrow(remote_files) == 0) return(invisible(0L))

  dir.create(local_dir, showWarnings = FALSE, recursive = TRUE)

  downloaded <- 0L
  for (i in seq_len(nrow(remote_files))) {
    local_path <- file.path(local_dir, remote_files$name[i])
    if (!file.exists(local_path)) {
      drive_download(remote_files[i, ], path = local_path, overwrite = FALSE)
      downloaded <- downloaded + 1L
    }
  }
  invisible(downloaded)
}

# Reads all CSVs in game_csv_dir, combines them, applies the same
# TaggedPitchType and Count transformations as global.R, and writes
# the result to output_path. Returns total row count (invisibly).
build_combined_csv <- function(game_csv_dir = GAME_CSV_DIR,
                                output_path  = "all_fall_25.csv") {
  csv_files <- list.files(game_csv_dir, pattern = "\\.csv$",
                           full.names = TRUE, recursive = FALSE)
  if (length(csv_files) == 0)
    stop("No CSV files found in ", game_csv_dir)

  combined <- dplyr::bind_rows(
    lapply(csv_files, readr::read_csv, show_col_types = FALSE)
  )

  combined$TaggedPitchType <- dplyr::if_else(
    combined$TaggedPitchType %in% c("Other", NA_character_),
    "Undefined",
    combined$TaggedPitchType
  )
  combined$Count <- paste0(combined$Balls, "-", combined$Strikes)

  readr::write_csv(combined, output_path)
  invisible(nrow(combined))
}
