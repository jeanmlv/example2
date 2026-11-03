#!/usr/bin/env Rscript
# ================================================================
# VEGA WK50 | load-2388 | SDTM -> UC variable discovery (STEP 01)
# Adapted from FIGARO 01_map_uc_variables_from_sdtm.R
# Discovery only: no ARD creation and no automatic clinical approval.
# ================================================================

options(stringsAsFactors = FALSE)

CONFIG <- list(
  study_id = "CNTO1959UCO2002_VEGA_WK50",
  load_root = "/domino/datasets/local/clinical-trial-data/CNTO1959UCO2002-VEGA-WK50/load-2388",
  # NULL: prefer Data/csv, but discover all nested folders for inventory.
  selected_source = "csv", # csv | cleaned
  output_root = "/mnt",
  output_subdir = "CNTO1959UCO2002_VEGA_load2388_mapping",
  max_candidates_per_variable = 15L
)

fail <- function(...) stop(paste0(...), call. = FALSE)
message("[VEGA] Starting STEP 01")
if (!dir.exists(CONFIG$load_root)) fail("Input not found: ", CONFIG$load_root)
if (!dir.exists(CONFIG$output_root)) fail("Output root not found: ", CONFIG$output_root)
output_dir <- file.path(CONFIG$output_root, CONFIG$output_subdir)
if (!dir.exists(output_dir) && !dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)) {
  fail("Cannot create output directory: ", output_dir)
}
probe <- file.path(output_dir, ".write_probe")
tryCatch(writeLines("ok", probe), error=function(e) fail("Output is not writable: ", conditionMessage(e)))
unlink(probe)

# The original FIGARO target dictionary is preserved, including its spelling.
voi <- data.frame(
  PARAMCD = c("MAYO","ENSCORE","RBSCORE","SFSCORE","ENCSCORE","ENLSCORE","UCEISP",
    "BLEEDP","EROSP","VASCP","CRESPP","HEHL","HIHL","ENHLP","CREMP",
    "PMRESPP","ARCHCH","CHRIINF","EOS","NEUTLP","NEUTEP","CRYPTDES",
    "ULCER","CRPP","CALPPP","CORTBL","IBDQTOT","PGSCORE","MOTEST",
    "IBREMP","HEHLP","ENNRMP","NANCYHI","RHI","GBTOT","GBHI","GBLO"),
  DESCRIPTION = c("Mayo Score","Endoscopy Final Score","Rectal Bleeding Score",
    "Stool Frequency Score","Endoscopy Central Score (Primary Reader)",
    "Endoscopy Local Score","Total UCEIS Score (Primary ICE Approach)",
    "Bleeding (Primary ICE Approach)","Erosions and Ulcers (Primary ICE Approach)",
    "Vascular Pattern (Primary ICE Approach)","Clin Resp (Primary Estimand Approach)",
    "Histo-Endo Mucosal Heal","Histo Healing",
    "Endoscopic Healing (Primary Estimand Approach)",
    "Clin Remiss (Primary Estimand Approach)",
    "Partial Mayo Resp (Primary ICE Approach)","Architectural Change (Grade 0)",
    "Chronic Inflammatory Infiltrate (Grade 1)","Eosinophils (10^9/L)",
    "Neutrophils Lamina Propria (Grade 2B)","Neutrophils Epithelium (Grade 3)",
    "Crypt Destruction (Grade 4)","Erosion or Ulceration (Grade 5)",
    "C Reactive Protein (mg/L) (Primary ICE Approach)",
    "Fecal Calprotectin (mg/kg) (Primary ICE Approach)",
    "Induction Baseline Corticosteroid Use","IBDQ Total Score",
    "Physician Global Assessment Score","Modified Mayo Score",
    "IBDQ Remission (Primary Estimand Approach)",
    "Histo-Endo Mucosal Heal (Primary Estimand Approach)",
    "Endoscopic Normalization (Primary Estimand Approach)",
    "Nancy Histological Index","Robarts Histopathology Index",
    "Geboes total score","Geboes high activity subscore","Geboes low activity subscore"),
  stringsAsFactors=FALSE
)
stopifnot(nrow(voi)==37L, !anyDuplicated(voi$PARAMCD))

aliases <- c(
  MAYO="total mayo", ENSCORE="endoscopy outcome", RBSCORE="rectal bleeding",
  SFSCORE="stool frequency", ENCSCORE="central read endoscopy mayo primary reader",
  ENLSCORE="local reader endoscopy mayo score", UCEISP="uceis uceissc",
  BLEEDP="bleeding uceis", EROSP="erosion ulcers uceis", VASCP="vascular pattern uceis",
  CRESPP="clinical response responder", HEHL="histo endo mucosal healing",
  HIHL="histological healing", ENHLP="endoscopic healing", CREMP="clinical remission",
  PMRESPP="partial mayo response", ARCHCH="architectural change", CHRIINF="chronic inflammatory infiltrate",
  EOS="eosinophils", NEUTLP="lamina propria neutrophils", NEUTEP="epithelial neutrophils",
  CRYPTDES="crypt destruction", ULCER="erosion ulceration", CRPP="c reactive protein crp",
  CALPPP="fecal calprotectin calpro", CORTBL="corticosteroid", IBDQTOT="ibdq total",
  PGSCORE="physician global assessment", MOTEST="modified mayo",
  IBREMP="ibdq remission", HEHLP="histo endo mucosal heal", ENNRMP="endoscopic normalization",
  NANCYHI="nancy histological index", RHI="robarts histopathology index",
  GBTOT="total geboes", GBHI="geboes high activity", GBLO="geboes low activity"
)

write_csv <- function(x, filename) {
  utils::write.csv(x, file.path(output_dir, filename), row.names=FALSE, na="", fileEncoding="UTF-8")
}
norm <- function(x) {
  x <- tolower(ifelse(is.na(x), "", as.character(x)))
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", x)))
}
get_domain <- function(path) {
  x <- tolower(basename(path))
  x <- sub("\\.clean\\.csv$", "", x)
  x <- sub("\\.sas7bdat\\.csv$", "", x)
  x <- sub("\\.csv$", "", x)
  x <- sub("\\.xpt$", "", x)
  toupper(x)
}
source_kind <- function(path) {
  p <- tolower(gsub("\\\\", "/", path))
  if (grepl("/cleaned/|\\.clean\\.csv$", p)) "cleaned" else
    if (grepl("/csv/", p)) "csv" else "other"
}
read_domain <- function(path) {
  if (grepl("\\.xpt$", path, ignore.case=TRUE)) {
    if (!requireNamespace("haven", quietly=TRUE)) fail("haven is required for XPT: ", path)
    x <- as.data.frame(haven::read_xpt(path), stringsAsFactors=FALSE)
  } else {
    # Base R is compatible with older readr versions on Domino.
    x <- utils::read.csv(path, check.names=FALSE, stringsAsFactors=FALSE,
      colClasses="character", na.strings=c("", "NA"), fileEncoding="UTF-8-BOM")
  }
  names(x) <- toupper(trimws(names(x)))
  x
}

all_files <- list.files(CONFIG$load_root, recursive=TRUE, full.names=TRUE,
  pattern="(\\.csv|\\.xpt)$", ignore.case=TRUE)
if (!length(all_files)) fail("No CSV/XPT files found recursively in: ", CONFIG$load_root)
files <- data.frame(PATH=all_files, DATASET=vapply(all_files,get_domain,""),
  SOURCE_KIND=vapply(all_files,source_kind,""), stringsAsFactors=FALSE)
files$FILE_NAME <- basename(files$PATH)
files$SIZE_BYTES <- file.info(files$PATH)$size
files$SELECTED <- FALSE
write_csv(files, "01_all_files_inventory.csv")

selected <- files[files$SOURCE_KIND==CONFIG$selected_source, ,drop=FALSE]
if (!nrow(selected)) fail("No files found for selected_source='", CONFIG$selected_source,
  "'. Review 01_all_files_inventory.csv and change CONFIG$selected_source.")
if (anyDuplicated(selected$DATASET)) {
  dup <- unique(selected$DATASET[duplicated(selected$DATASET)])
  write_csv(selected[selected$DATASET %in% dup,,drop=FALSE], "01_duplicate_domain_files.csv")
  fail("Duplicate domains within selected source: ", paste(dup,collapse=", "),
    ". Inspect 01_duplicate_domain_files.csv; do not merge loads automatically.")
}
selected <- selected[order(selected$DATASET),,drop=FALSE]
files$SELECTED <- files$PATH %in% selected$PATH
write_csv(files, "01_all_files_inventory.csv")
write_csv(selected, "01_selected_files.csv")
write_csv(voi, "00_37_variables_of_interest.csv")

metadata_list <- list(); column_list <- list(); inventory_list <- list(); error_list <- list()
for (i in seq_len(nrow(selected))) {
  domain <- selected$DATASET[i]; path <- selected$PATH[i]
  message(sprintf("[VEGA] Reading %s (%d/%d): %s",domain,i,nrow(selected),basename(path)))
  df <- tryCatch(read_domain(path), error=function(e) {
    error_list[[length(error_list)+1L]] <<- data.frame(DATASET=domain, PATH=path,
      ERROR=conditionMessage(e)); NULL
  })
  if (is.null(df)) next
  nms <- names(df)
  inventory_list[[length(inventory_list)+1L]] <- data.frame(
    DATASET=domain, N_ROWS=nrow(df), N_COLS=ncol(df),
    N_SUBJECTS=if ("USUBJID" %in% nms) length(unique(stats::na.omit(df$USUBJID))) else NA_integer_,
    SOURCE_KIND=selected$SOURCE_KIND[i], SOURCE_PATH=path,
    VARIABLES=paste(nms,collapse=" | "))
  column_list[[length(column_list)+1L]] <- data.frame(DATASET=domain,
    COLUMN=nms, SOURCE_PATH=path)
  for (tc in grep("TESTCD$", nms, value=TRUE)) {
    label_col <- sub("TESTCD$", "TEST", tc)
    if (!(label_col %in% nms)) next
    keep <- !is.na(df[[tc]]) | !is.na(df[[label_col]])
    if (!any(keep)) next
    pair <- unique(data.frame(CODE=as.character(df[[tc]][keep]),
      LABEL=as.character(df[[label_col]][keep]), stringsAsFactors=FALSE))
    for (j in seq_len(nrow(pair))) {
      hits <- keep & !is.na(df[[tc]]) & df[[tc]]==pair$CODE[j]
      metadata_list[[length(metadata_list)+1L]] <- data.frame(
        DATASET=domain, METADATA_TYPE="TEST", CODE_VAR=tc,
        CODE=pair$CODE[j], LABEL_VAR=label_col, LABEL=pair$LABEL[j],
        N_RECORDS=sum(hits), N_SUBJECTS=if ("USUBJID" %in% nms)
          length(unique(stats::na.omit(df$USUBJID[hits]))) else NA_integer_,
        SOURCE_PATH=path)
    }
  }
  if (all(c("QNAM","QLABEL") %in% nms)) {
    pair <- unique(df[!is.na(df$QNAM) | !is.na(df$QLABEL), c("QNAM","QLABEL"),drop=FALSE])
    for (j in seq_len(nrow(pair))) {
      hits <- !is.na(df$QNAM) & df$QNAM==pair$QNAM[j]
      metadata_list[[length(metadata_list)+1L]] <- data.frame(
        DATASET=domain, METADATA_TYPE="SUPP", CODE_VAR="QNAM",
        CODE=pair$QNAM[j], LABEL_VAR="QLABEL", LABEL=pair$QLABEL[j],
        N_RECORDS=sum(hits), N_SUBJECTS=if ("USUBJID" %in% nms)
          length(unique(stats::na.omit(df$USUBJID[hits]))) else NA_integer_,
        SOURCE_PATH=path)
    }
  }
  rm(df); if (i %% 5L == 0L) gc(verbose=FALSE)
}
if (length(error_list)) write_csv(do.call(rbind,error_list),"01_read_errors.csv")
if (!length(inventory_list)) fail("No selected SDTM datasets could be read.")
inventory <- do.call(rbind,inventory_list)
write_csv(inventory,"02_sdtm_inventory.csv")
columns <- do.call(rbind,column_list)
write_csv(columns,"03_sdtm_columns.csv")
if (!length(metadata_list)) fail("No TESTCD/TEST or QNAM/QLABEL metadata found.")
metadata <- do.call(rbind,metadata_list)
metadata$SEARCH_TEXT <- norm(paste(metadata$CODE,metadata$LABEL))
write_csv(metadata,"04_sdtm_test_and_supp_metadata.csv")

# Scores are discovery evidence only; a high score is NOT clinical approval.
stopwords <- c("primary","approach","estimand","score","total","grade","clinical",
  "the","and","for","final","test","with","activity","index","observed")
tokens <- function(s) {
  x <- unlist(strsplit(norm(s)," ",fixed=TRUE))
  unique(setdiff(x[nchar(x)>=3L],stopwords))
}
score <- function(target, candidate) {
  a <- tokens(target); b <- tokens(candidate)
  if (!length(a) || !length(b)) return(0)
  length(intersect(a,b))/length(a)
}

# Add explicit hints as candidates, not approvals; they may differ by load.
hints <- list(
  ENCSCORE=c("MCSSUBSC","MCSSUBFN","MAYOSUBS"),
  ENLSCORE=c("SCORE","ENMAYO"),
  UCEISP=c("UCEISSC","UCEIS"),
  BLEEDP=c("BLEEDING"), EROSP=c("EROSION"), VASCP=c("VASCULAR")
)
derived <- c("UCEISP","BLEEDP","EROSP","VASCP","CRESPP","HEHL","HIHL",
  "ENHLP","CREMP","PMRESPP","CRPP","CALPPP","IBREMP","HEHLP","ENNRMP",
  "GBHI","GBLO")

candidate_list <- list()
for (i in seq_len(nrow(voi))) {
  p <- voi$PARAMCD[i]; description <- voi$DESCRIPTION[i]
  phrase <- paste(description, aliases[[p]])
  sc <- vapply(seq_len(nrow(metadata)), function(j) score(phrase,metadata$SEARCH_TEXT[j]),numeric(1))
  exact <- toupper(ifelse(is.na(metadata$CODE),"",metadata$CODE))==p
  hint <- if (!is.null(hints[[p]])) toupper(ifelse(is.na(metadata$CODE),"",metadata$CODE)) %in% hints[[p]] else rep(FALSE,nrow(metadata))
  rank <- sc + 2*as.numeric(exact) + 0.75*as.numeric(hint)
  inds <- which(sc>0 | exact | hint)
  if (!length(inds)) next
  inds <- inds[order(-rank[inds],-metadata$N_RECORDS[inds],na.last=TRUE)]
  inds <- head(inds, CONFIG$max_candidates_per_variable)
  x <- metadata[inds,,drop=FALSE]
  x$PARAMCD <- p; x$DESCRIPTION <- description
  x$SCORE <- round(sc[inds],3)
  x$EXACT_PARAMCD_MATCH <- exact[inds]
  x$VEGA_HINT <- hint[inds]
  x$TARGET_REQUIRES_ANALYTICAL_REVIEW <- p %in% derived
  x$AUTO_TRIAGE <- ifelse(exact[inds],"EXACT_CODE_CANDIDATE",
    ifelse(hint[inds],"VEGA_CODE_HINT_REVIEW",
      ifelse(sc[inds]>=0.6,"STRONG_TEXT_CANDIDATE","TEXT_CANDIDATE_REVIEW")))
  x$MAPPING_STATUS <- "REVIEW_REQUIRED"
  candidate_list[[length(candidate_list)+1L]] <- x
}
candidates <- if (length(candidate_list)) do.call(rbind,candidate_list) else data.frame()
write_csv(candidates,"05_mapping_candidates_automatic.csv")

# Full 37-row overview, intentionally not approved for downstream ARD extraction.
overview_list <- lapply(seq_len(nrow(voi)),function(i) {
  p <- voi$PARAMCD[i]
  z <- if (nrow(candidates)) candidates[candidates$PARAMCD==p,,drop=FALSE] else data.frame()
  if (nrow(z)) {
    x <- z[1,,drop=FALSE]
    data.frame(PARAMCD=p, DESCRIPTION=voi$DESCRIPTION[i],
      AUTO_TRIAGE=x$AUTO_TRIAGE, AUTO_METADATA_TYPE=x$METADATA_TYPE,
      AUTO_DATASET=x$DATASET, AUTO_CODE_VAR=x$CODE_VAR,
      AUTO_CODE=x$CODE, AUTO_LABEL_VAR=x$LABEL_VAR, AUTO_LABEL=x$LABEL,
      AUTO_SCORE=x$SCORE, AUTO_EXACT_CODE_MATCH=x$EXACT_PARAMCD_MATCH,
      N_CANDIDATES=nrow(z), TARGET_REQUIRES_ANALYTICAL_REVIEW=p %in% derived,
      REVIEW_STATUS="REVIEW_REQUIRED", REVIEW_NOTES="", stringsAsFactors=FALSE)
  } else data.frame(PARAMCD=p, DESCRIPTION=voi$DESCRIPTION[i],
    AUTO_TRIAGE="NO_AUTOMATIC_CANDIDATE", AUTO_METADATA_TYPE=NA_character_,
    AUTO_DATASET=NA_character_, AUTO_CODE_VAR=NA_character_, AUTO_CODE=NA_character_,
    AUTO_LABEL_VAR=NA_character_, AUTO_LABEL=NA_character_, AUTO_SCORE=NA_real_,
    AUTO_EXACT_CODE_MATCH=FALSE, N_CANDIDATES=0L,
    TARGET_REQUIRES_ANALYTICAL_REVIEW=p %in% derived,
    REVIEW_STATUS="NOT_IDENTIFIED", REVIEW_NOTES="", stringsAsFactors=FALSE)
})
overview <- do.call(rbind,overview_list)
write_csv(overview,"06_mapping_overview_best_candidate.csv")

# Reviewer may choose a candidate different from top-ranked one.
review <- if (nrow(candidates)) candidates else data.frame()
if (nrow(review)) {
  review$APPROVED <- "NO"
  review$REVIEWER_NOTES <- ""
  write_csv(review,"07_mapping_review_template.csv")
}
summary <- data.frame(METRIC=c("FILES_DISCOVERED","FILES_SELECTED","DATASETS_READ",
  "METADATA_ENTRIES","TARGET_VARIABLES","TARGETS_WITH_CANDIDATES",
  "TARGETS_WITHOUT_CANDIDATES"),
  VALUE=c(nrow(files),nrow(selected),nrow(inventory),nrow(metadata),nrow(voi),
    sum(overview$N_CANDIDATES>0),sum(overview$N_CANDIDATES==0)))
write_csv(summary,"08_execution_summary.csv")
cat("\n=== VEGA STEP 01 COMPLETED ===\n")
print(summary,row.names=FALSE)
cat("Output: ",output_dir,"\n",sep="")
cat("IMPORTANT: Candidate mappings are not approved. Do not use script 02 unchanged.\n")
