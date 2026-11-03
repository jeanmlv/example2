# example2


# Identificar diretórios com permissão de escrita
paths <- unique(c(
  getwd(),
  Sys.getenv("HOME"),
  "/mnt/data",
  tempdir()
))

for (p in paths) {
  if (!dir.exists(p)) next

  test_file <- file.path(p, "vega_write_test.txt")

  ok <- tryCatch({
    writeLines("test", test_file)
    unlink(test_file)
    TRUE
  }, error = function(e) FALSE)

  cat(p, "->", if (ok) "WRITABLE" else "NOT WRITABLE", "\n")
}









CONFIG <- list(
  study_id = "CNTO1959UCO2002_VEGA",

  input_dir = paste0(
    "/domino/datasets/local/clinical-trial-data/",
    "CNTO1959UCO2002-VEGA-WK50/",
    "load-2388/Data/csv"
  ),

  output_dir = file.path(
    getwd(),
    "vega_load2388_ard"
  )
)

study_id <- CONFIG$study_id
sdtm_root <- CONFIG$input_dir
output_dir <- CONFIG$output_dir

reviewed_mapping_file <- file.path(
  output_dir,
  "05_reviewed_mapping_template.csv"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(
  dir.exists(sdtm_root),
  dir.exists(output_dir),
  file.access(output_dir, 2) == 0
)
