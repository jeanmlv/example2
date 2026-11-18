#!/usr/bin/env Rscript
# STARDUST ADaM metadata evidence audit - v6 fixed
# Rscript stardust_metadata_gap_v6_fixed.R [input.csv|input.xlsx] [metadata_dir] [output_dir]
# Requires readxl. Reads metadata only; never exports patient-level data.
suppressPackageStartupMessages(library(readxl))
args <- commandArgs(trailingOnly = TRUE)
root <- '/domino/datasets/local/clinical-trial-data/CNTO1275CRD3005-STARDUST/load-1884/Metadata'
input_path <- if (length(args) >= 1L) args[[1L]] else file.path(root, 'scripts', 'stardust_variables_of_interest.csv')
meta_dir <- if (length(args) >= 2L) args[[2L]] else root
out_dir <- if (length(args) >= 3L) args[[3L]] else file.path(root, 'stardust_metadata_gap_results_v6')
if (!file.exists(input_path)) stop('Input not found: ', input_path)
if (!dir.exists(meta_dir)) stop('Metadata directory not found: ', meta_dir)

as_text <- function(x) { x <- as.character(x); x[is.na(x)] <- ''; x }
code_norm <- function(x) toupper(gsub('[^A-Za-z0-9]', '', as_text(x)))
text_norm <- function(x) {
  x <- iconv(as_text(x), from = '', to = 'ASCII//TRANSLIT', sub = '')
  x[is.na(x)] <- ''
  toupper(trimws(gsub('[^A-Za-z0-9]+', ' ', x)))
}
read_input <- function(path) {
  if (grepl('\\.xlsx?$', path, ignore.case = TRUE)) {
    return(as.data.frame(readxl::read_excel(path, sheet = 'STARDUST', .name_repair = 'minimal'), check.names = FALSE))
  }
  first <- readLines(path, n = 1L, warn = FALSE, encoding = 'UTF-8')
  sep <- if (length(first) && grepl(';', first, fixed = TRUE) && !grepl(',', first, fixed = TRUE)) ';' else ','
  read.csv(path, sep = sep, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')
}
inp <- read_input(input_path)
names(inp) <- tolower(trimws(sub('^\\ufeff', '', names(inp))))
required <- c('variable_of_interest', 'name')
if (!all(required %in% names(inp))) stop('Missing required columns. Found: ', paste(names(inp), collapse = ', '))
inp$variable_code <- code_norm(inp$variable_of_interest)
inp$variable_label <- as_text(inp$name)
inp <- inp[nzchar(inp$variable_code), , drop = FALSE]
if (nrow(inp) == 0L) stop('No valid input variables')
inp$input_row <- seq_len(nrow(inp))

files <- list.files(meta_dir, pattern = '\\.xlsx?$', full.names = TRUE, ignore.case = TRUE, recursive = FALSE)
if (length(files) == 0L) stop('No metadata Excel files found')
# Metadata sheets may be different sizes. Read actual used area, preserving A1 origin.
# Large sheets are capped and flagged INCOMPLETE; no silent negative conclusions.
max_rows <- 5000L
max_cols <- 100L
cells_list <- list(); diag_list <- list(); cell_i <- 0L; diag_i <- 0L
for (f in files) {
  sheets <- tryCatch(readxl::excel_sheets(f), error = function(e) { warning(conditionMessage(e)); character() })
  if (length(sheets) == 0L) {
    diag_i <- diag_i + 1L
    diag_list[[diag_i]] <- data.frame(file = basename(f), sheet = '', status = 'ERROR', rows_read = 0L, columns_read = 0L, message = 'No readable sheets')
  }
  for (s in sheets) {
    err <- ''
    tab <- tryCatch(suppressWarnings(readxl::read_excel(f, sheet = s,
      range = readxl::cell_limits(c(1, 1), c(max_rows, max_cols)),
      col_names = FALSE, col_types = 'text', .name_repair = 'minimal')),
      error = function(e) { err <<- conditionMessage(e); NULL })
    diag_i <- diag_i + 1L
    diag_list[[diag_i]] <- data.frame(file = basename(f), sheet = s,
      status = if (is.null(tab)) 'ERROR' else 'OK_LIMITED_RANGE',
      rows_read = if (is.null(tab)) 0L else nrow(tab),
      columns_read = if (is.null(tab)) 0L else ncol(tab), message = err)
    if (is.null(tab) || nrow(tab) == 0L || ncol(tab) == 0L) next
    mat <- as.matrix(tab); mat[is.na(mat)] <- ''
    occupied <- matrix(nzchar(trimws(as.vector(mat))), nrow = nrow(mat), ncol = ncol(mat))
    indices <- which(occupied, arr.ind = TRUE)
    if (NROW(indices) == 0L) next
    for (j in seq_len(nrow(indices))) {
      r <- indices[j, 1L]; cc <- indices[j, 2L]
      row_values <- as_text(mat[r, ])
      context <- paste(row_values[nzchar(trimws(row_values))], collapse = ' | ')
      cell_i <- cell_i + 1L
      cells_list[[cell_i]] <- data.frame(file = basename(f),
        dataset = toupper(sub('\\.xlsx?$', '', basename(f), ignore.case = TRUE)),
        sheet = s, row = r, column = cc, value = as_text(mat[r, cc]),
        row_context = substr(context, 1L, 3000L), stringsAsFactors = FALSE)
    }
  }
}
if (length(cells_list) == 0L) stop('No nonempty metadata cells read; no reports written')
cells <- do.call(rbind, cells_list)
diag <- do.call(rbind, diag_list)
# Explicit structure flags are deliberately conservative; no assumption that
# every workbook uses the same layout.
classify <- function(sheet, col, val, query_code, exact_cell) {
  sh <- toupper(trimws(sheet))
  if (exact_cell && sh == 'ADM CODELIST' && col == 8L) return('CODELIST_CODE_CANDIDATE')
  if (exact_cell && sh %in% c('ADEFF','ADQS','ADSL','ADAE','ADCM','ADLB','ADMH','ADPC','ADVS','ADTE','ADTFTE','ADDDV','ADDX','ADDISP','ADLBS') && col == 7L) return('VARIABLE_DEFINITION_CANDIDATE')
  if (grepl('PARAMCD|LBTESTCD|DERIV|IF |THEN |ELSE |SET TO', toupper(val))) return('DERIVATION_OR_PARAMETER_REFERENCE')
  if (sh == 'ADM CODELIST') return('CODELIST_OTHER_REFERENCE')
  'OTHER_METADATA_REFERENCE'
}
synonyms <- list(
  CALPRO = c('FECAL CALPROTECTIN','FAECAL CALPROTECTIN'),
  CRP = c('C REACTIVE PROTEIN'),
  SESCDTOT = c('SES CD TOTAL SCORE','SIMPLE ENDOSCOPIC SCORE'),
  CDAI = c('CROHN S DISEASE ACTIVITY INDEX'),
  CDAITOT = c('CROHN S DISEASE ACTIVITY INDEX'),
  FC = c('FECAL CALPROTECTIN','FAECAL CALPROTECTIN')
)
# Token boundaries in raw text; avoid substring false positives.
# Escape is unnecessary: input codes contain only A-Z and digits.
values_upper <- toupper(cells$value)
values_norm <- text_norm(cells$value)
evidence_list <- list(); summary_list <- vector('list', nrow(inp)); e_i <- 0L
for (i in seq_len(nrow(inp))) {
  q <- inp$variable_code[[i]]
  token <- paste0('(^|[^A-Z0-9])', q, '($|[^A-Z0-9])')
  code_hit <- grepl(token, values_upper, perl = TRUE)
  exact_cell <- code_norm(cells$value) == q
  label <- text_norm(inp$variable_label[[i]])
  terms <- character()
  if (nchar(label) >= 12L && grepl(' ', label, fixed = TRUE)) terms <- c(terms, label)
  if (!is.null(synonyms[[q]])) terms <- c(terms, synonyms[[q]])
  terms <- unique(text_norm(terms))
  desc_hit <- rep(FALSE, nrow(cells))
  for (term in terms) if (nchar(term) >= 8L) desc_hit <- desc_hit | grepl(term, values_norm, fixed = TRUE)
  idx <- which(code_hit | desc_hit)
  ev <- NULL
  if (length(idx) > 0L) {
    ev <- cells[idx, , drop = FALSE]
    ev$variable_of_interest <- q
    ev$match_basis <- ifelse(exact_cell[idx], 'EXACT_CELL_CODE', ifelse(code_hit[idx], 'CODE_TOKEN_IN_TEXT', 'DESCRIPTION_CANDIDATE'))
    ev$evidence_type <- vapply(seq_along(idx), function(j) {
      if (!code_hit[idx[j]]) return('CLINICAL_CONCEPT_CANDIDATE')
      classify(ev$sheet[[j]], ev$column[[j]], ev$value[[j]], q, exact_cell[idx[j]])
    }, character(1L))
    ev$review_required <- TRUE
    e_i <- e_i + 1L
    evidence_list[[e_i]] <- ev[, c('variable_of_interest','dataset','file','sheet','row','column','match_basis','evidence_type','value','row_context','review_required')]
  }
  types <- if (is.null(ev)) character() else unique(ev$evidence_type)
  level <- if ('VARIABLE_DEFINITION_CANDIDATE' %in% types) 'VARIABLE_DEFINITION_CANDIDATE' else
    if ('CODELIST_CODE_CANDIDATE' %in% types) 'CODELIST_CODE_CANDIDATE' else
    if ('DERIVATION_OR_PARAMETER_REFERENCE' %in% types) 'DERIVATION_OR_PARAMETER_REFERENCE' else
    if ('CLINICAL_CONCEPT_CANDIDATE' %in% types) 'CLINICAL_CONCEPT_CANDIDATE' else
    if (length(types) > 0L) 'OTHER_METADATA_REFERENCE' else 'NOT_IDENTIFIED_IN_SCANNED_METADATA'
  if (any(diag$status != 'OK_LIMITED_RANGE') && length(idx) == 0L) level <- 'INCOMPLETE_METADATA_REVIEW'
  previous <- if ('available? (y/n)' %in% names(inp)) as_text(inp[['available? (y/n)']][i]) else ''
  summary_list[[i]] <- data.frame(input_row = inp$input_row[[i]], variable_of_interest = q,
    name = inp$variable_label[[i]], previous_status = previous,
    evidence_level = level, evidence_count = length(idx),
    candidate_datasets = if (length(idx) > 0L) paste(unique(ev$dataset), collapse = '; ') else '',
    metadata_definition_confirmed = 'REQUIRES_HUMAN_VALIDATION',
    dataset_presence_confirmed = 'NOT_TESTED', clinical_collection_confirmed = 'NOT_TESTED',
    review_recommendation = if (length(idx) == 0L)
      'No match in scanned metadata; verify complete metadata and actual ADaM/SDTM before marking unavailable'
      else 'Inspect cited sheet/row/column and verify actual ADaM PARAMCD and data records',
    stringsAsFactors = FALSE)
}
res <- do.call(rbind, summary_list)
ev <- if (length(evidence_list) > 0L) do.call(rbind, evidence_list) else
  data.frame(variable_of_interest = character(), dataset = character(), file = character(), sheet = character(), row = integer(), column = integer(), match_basis = character(), evidence_type = character(), value = character(), row_context = character(), review_required = logical())
if (nrow(res) != nrow(inp)) stop('Internal error: summary/input mismatch')
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(out_dir)) stop('Cannot create output directory: ', out_dir)
write.csv(res, file.path(out_dir, 'stardust_gap_summary_v6.csv'), row.names = FALSE, na = '')
write.csv(ev, file.path(out_dir, 'stardust_metadata_evidence_v6.csv'), row.names = FALSE, na = '')
write.csv(diag, file.path(out_dir, 'stardust_read_diagnostics_v6.csv'), row.names = FALSE, na = '')
write.csv(as.data.frame(table(res$evidence_level), stringsAsFactors = FALSE), file.path(out_dir, 'stardust_status_counts_v6.csv'), row.names = FALSE)
cat('Input variables:', nrow(inp), '\nMetadata files:', length(files), '\nMetadata cells:', nrow(cells),
    '\nSummary rows:', nrow(res), '\nEvidence rows:', nrow(ev),
    '\nRead errors:', sum(diag$status == 'ERROR'), '\nOutput:', out_dir, '\n')
print(table(res$evidence_level))
cat('IMPORTANT: All structural matches are candidates until metadata layout is validated. Metadata alone does not prove collection or actual dataset records.\n')
