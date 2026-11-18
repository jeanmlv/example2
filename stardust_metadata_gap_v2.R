#!/usr/bin/env Rscript
# STARDUST ADaM metadata gap assessment (read-only)
# Run: Rscript stardust_metadata_gap_v2.R /path/cd_variables_gap_analysis.xlsx
# Optional: Rscript ... workbook.xlsx /path/to/Metadata /path/to/output
suppressPackageStartupMessages(library(readxl))
args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop('Provide path to cd_variables_gap_analysis.xlsx')
workbook <- args[1]
meta_dir <- if(length(args)>=2) args[2] else '/domino/datasets/local/clinical-trial-data/CNTO1275CRD3005-STARDUST/load-1884/Metadata'
out_dir <- if(length(args)>=3) args[3] else file.path(getwd(),'stardust_metadata_gap_results')
if (!file.exists(workbook)) stop('Workbook not found: ', workbook)
if (!dir.exists(meta_dir)) stop('Metadata folder not found: ', meta_dir)
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
normalize <- function(x) {x <- toupper(trimws(as.character(x))); x[is.na(x)] <- ''; gsub('^[[:space:]"\x27,;]+|[[:space:]"\x27,;]+$', '', x)}
clean <- function(x) {x <- as.character(x); x[is.na(x)] <- ''; trimws(x)}
inputs <- as.data.frame(readxl::read_excel(workbook, sheet='STARDUST', col_types='text', .name_repair='unique'), stringsAsFactors=FALSE)
required <- c('variable_of_interest','name','name(s) in ard dataset','available? (Y/N)')
if (!all(required %in% names(inputs))) stop('Missing columns: ', paste(setdiff(required,names(inputs)),collapse=', '))
inputs$variable_code <- normalize(inputs[['variable_of_interest']])
inputs$variable_label <- clean(inputs[['name']])
inputs$previous_ard_name <- clean(inputs[['name(s) in ard dataset']])
inputs$previous_status <- normalize(inputs[['available? (Y/N)']])
inputs$input_row <- seq_len(nrow(inputs)) + 1L
inputs <- inputs[nzchar(inputs$variable_code),,drop=FALSE]
files <- sort(list.files(meta_dir, pattern='\\.xls[xm]?$', full.names=TRUE, ignore.case=TRUE))
if (!length(files)) stop('No Excel metadata files found in ', meta_dir)
# Capture every non-empty cell, with row/column coordinates; do not assume a template layout.
all_cells <- list(); diagnostics <- list(); k <- 0L
for (f in files) {
  sheets <- tryCatch(readxl::excel_sheets(f), error=function(e) {warning('Cannot open ',basename(f),': ',conditionMessage(e)); character()})
  if (!length(sheets)) {diagnostics[[length(diagnostics)+1L]] <- data.frame(file=basename(f),sheet='',cells=0,read_ok=FALSE);next}
  for (sh in sheets) {
    tab <- tryCatch(suppressWarnings(as.data.frame(readxl::read_excel(f,sheet=sh,col_names=FALSE,col_types='text',.name_repair='minimal',trim_ws=FALSE),stringsAsFactors=FALSE)),error=function(e) {warning('Cannot read ',basename(f),' / ',sh,': ',conditionMessage(e));NULL})
    if (is.null(tab)) {diagnostics[[length(diagnostics)+1L]] <- data.frame(file=basename(f),sheet=sh,cells=0,read_ok=FALSE);next}
    if (!nrow(tab) || !ncol(tab)) {diagnostics[[length(diagnostics)+1L]] <- data.frame(file=basename(f),sheet=sh,cells=0,read_ok=TRUE);next}
    # Read with col_names=FALSE: readxl may skip leading blank rows. Coordinates are approximate if skipped.
    m <- as.matrix(tab); idx <- which(!is.na(m) & nzchar(trimws(m)),arr.ind=TRUE)
    diagnostics[[length(diagnostics)+1L]] <- data.frame(file=basename(f),sheet=sh,cells=nrow(idx),read_ok=TRUE)
    if (!nrow(idx)) next
    k <- k+1L
    all_cells[[k]] <- data.frame(file=basename(f),sheet=sh,row=idx[,1],column=idx[,2],value=as.character(m[idx]),stringsAsFactors=FALSE)
  }
}
if (!length(all_cells)) stop('No metadata cells could be read; check readxl and source files.')
cells <- do.call(rbind,all_cells)
cells$norm <- normalize(cells$value)
# Context: adjacent cells in same metadata row (evidence only; not asserted PARAMCD structure)
row_keys <- paste(cells$file,cells$sheet,cells$row,sep='\r')
row_text <- tapply(cells$value,row_keys,function(v) paste(unique(v),collapse=' | '))
cells$context <- unname(row_text[row_keys])
# Strict code match: entire cell or token boundary; avoid substring false positives (e.g. CRP inside longer code)
code_hit <- function(code, v) {
  if (!nzchar(code)) return(rep(FALSE,length(v)))
  # Codes are treated as complete tokens, never arbitrary substrings.
  vapply(strsplit(v, '[^A-Z0-9_]+', perl=TRUE), function(tokens) code %in% tokens, logical(1))
}
# Label match is candidate only, not proof. Require >= 7 chars to reduce generic matches.
label_hit <- function(label, v) {
  label <- normalize(label)
  if (nchar(label)<7 || label %in% c('NOT AVAILABLE','N/A','NA')) return(rep(FALSE,length(v)))
  grepl(label,v,fixed=TRUE)
}
res <- list(); evidence <- list()
for (i in seq_len(nrow(inputs))) {
  x <- inputs[i,]; code <- x$variable_code; label <- x$variable_label
  exact <- code_hit(code,cells$norm)
  descriptive <- label_hit(label,cells$norm) & !exact
  ix <- which(exact | descriptive)
  if (length(ix)) {
    matches <- cells[ix,c('file','sheet','row','column','value','context'),drop=FALSE]
    matches$variable_code <- code; matches$input_row <- x$input_row
    matches$match_type <- ifelse(exact[ix],'CODE_MATCH','DESCRIPTION_CANDIDATE')
    evidence[[length(evidence)+1L]] <- matches[,c('input_row','variable_code','match_type','file','sheet','row','column','value','context')]
  }
  status <- if(any(exact)) 'CODE_MATCH_IN_METADATA' else if(any(descriptive)) 'REVIEW_DESCRIPTION_CANDIDATE' else 'NOT_FOUND_IN_METADATA'
  res[[i]] <- data.frame(input_row=x$input_row,variable_code=code,variable_label=label,
    previous_ard_name=x$previous_ard_name,previous_status=x$previous_status,
    metadata_status=status,code_match_count=sum(exact),description_candidate_count=sum(descriptive),
    evidence_files=if(length(ix)) paste(unique(cells$file[ix]),collapse='; ') else '',
    interpretation='Metadata match does not confirm collected data; verify ADaM PARAMCD/PARAM and/or SDTM.',stringsAsFactors=FALSE)
}
summary <- do.call(rbind,res)
write.csv(summary,file.path(out_dir,'stardust_gap_summary.csv'),row.names=FALSE,na='')
if(length(evidence)) {ev <- do.call(rbind,evidence)} else {ev <- data.frame(input_row=integer(),variable_code=character(),match_type=character(),file=character(),sheet=character(),row=integer(),column=integer(),value=character(),context=character())}
write.csv(ev,file.path(out_dir,'stardust_metadata_evidence.csv'),row.names=FALSE,na='')
write.csv(cells[,c('file','sheet','row','column','value','context')],file.path(out_dir,'stardust_metadata_cells.csv'),row.names=FALSE,na='')
if(length(diagnostics)) write.csv(do.call(rbind,diagnostics),file.path(out_dir,'stardust_read_diagnostics.csv'),row.names=FALSE,na='')
cat('\nSTARDUST metadata gap assessment complete\n')
cat('Variables:',nrow(summary),' | Metadata files:',length(files),' | Cells:',nrow(cells),'\n')
print(table(summary$metadata_status))
cat('Output:',normalizePath(out_dir),'\n')
