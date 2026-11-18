#!/usr/bin/env Rscript
# STARDUST metadata gap assessment v5 - run with Rscript, not line by line.
# Usage: Rscript stardust_metadata_gap_v5.R /full/path/to/input.csv
# Or:    Rscript stardust_metadata_gap_v5.R /full/path/to/cd_variables_gap_analysis.xlsx
suppressPackageStartupMessages(library(readxl))
args <- commandArgs(trailingOnly=TRUE)
metadata_dir <- '/domino/datasets/local/clinical-trial-data/CNTO1275CRD3005-STARDUST/load-1884/Metadata'
input_file <- if(length(args)>=1) args[1] else file.path(metadata_dir,'scripts','stardust_variables_of_interest.csv')
output_dir <- if(length(args)>=2) args[2] else file.path(metadata_dir,'stardust_metadata_gap_results_v5')
if(!file.exists(input_file)) stop('Input not found: ',input_file)
if(!dir.exists(metadata_dir)) stop('Metadata folder not found: ',metadata_dir)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
clean <- function(x) { x <- as.character(x); x[is.na(x)] <- ''; trimws(x) }
codeclean <- function(x) { x <- toupper(gsub('[^A-Za-z0-9_]','',clean(x))); x }
norm <- function(x) { x <- iconv(clean(x),to='ASCII//TRANSLIT',sub=''); x[is.na(x)] <- ''; toupper(gsub('[^A-Za-z0-9]+',' ',x)) }
read_input <- function(p) {
 if(grepl('\\.xlsx?$',p,ignore.case=TRUE)) return(as.data.frame(readxl::read_excel(p,sheet='STARDUST',.name_repair='minimal'),check.names=FALSE))
 # Detect delimiter and handle UTF-8 BOM
 first <- readLines(p,n=1,warn=FALSE,encoding='UTF-8')
 sep <- if(length(first) && grepl(';',first,fixed=TRUE) && !grepl(',',first,fixed=TRUE)) ';' else ','
 read.csv(p,sep=sep,check.names=FALSE,fileEncoding='UTF-8-BOM',stringsAsFactors=FALSE)
}
inp <- read_input(input_file)
names(inp) <- trimws(sub('^\\ufeff','',names(inp)))
if(!all(c('variable_of_interest','name') %in% names(inp))) stop('Missing columns. Found: ',paste(names(inp),collapse=' | '))
inp$variable_code <- codeclean(inp[['variable_of_interest']])
inp$variable_label <- clean(inp[['name']])
inp <- inp[nzchar(inp$variable_code),,drop=FALSE]
if(!nrow(inp)) stop('No input variables after cleaning. Inspect the CSV.')
inp$input_row <- seq_len(nrow(inp))
cat('Input variables:',nrow(inp),'\n')
files <- list.files(metadata_dir,pattern='\\.xlsx?$',full.names=TRUE,ignore.case=TRUE)
if(!length(files)) stop('No XLS/XLSX metadata files found')
cat('Metadata files:',length(files),'\n')
# Read every sheet as a text matrix. Preserve sheet row and column coordinates.
cells <- list(); diagnostics <- list(); idx <- 0L
for(f in files) {
 sheets <- tryCatch(readxl::excel_sheets(f),error=function(e) {warning(conditionMessage(e)); character()})
 for(s in sheets) {
  mat <- tryCatch(as.data.frame(readxl::read_excel(f,sheet=s,col_names=FALSE,col_types='text',.name_repair='minimal'),stringsAsFactors=FALSE),error=function(e) {warning(conditionMessage(e));NULL})
  if(is.null(mat)) next
  diagnostics[[length(diagnostics)+1L]] <- data.frame(file=basename(f),sheet=s,rows=nrow(mat),columns=ncol(mat))
  if(!nrow(mat) || !ncol(mat)) next
  for(r in seq_len(nrow(mat))) {
   vals <- clean(unlist(mat[r,,drop=TRUE],use.names=FALSE))
   nonempty <- which(nzchar(vals))
   if(!length(nonempty)) next
   context <- paste(unique(vals[nonempty]),collapse=' | ')
   for(k in nonempty) {
    idx <- idx+1L
    cells[[idx]] <- data.frame(file=basename(f),sheet=s,row=r,column=k,value=vals[k],context=context,stringsAsFactors=FALSE)
   }
  }
 }
}
if(!length(cells)) stop('No readable metadata cells found')
allcells <- do.call(rbind,cells)
allcells$code <- codeclean(allcells$value)
allcells$normalized <- norm(allcells$value)
cat('Metadata cells:',nrow(allcells),'\n')
# Match exact code as a complete alphanumeric token, not a substring.
# Description match is only a candidate; no automatic clinical availability conclusion.
evidence <- list(); summaries <- list()
for(i in seq_len(nrow(inp))) {
 cd <- inp$variable_code[i]; label <- norm(inp$variable_label[i])
 # Vectorized token-boundary check on raw cell value
 exact <- grepl(paste0('(^|[^A-Za-z0-9_])',cd,'($|[^A-Za-z0-9_])'),toupper(allcells$value),perl=TRUE)
 descriptive <- rep(FALSE,nrow(allcells))
 if(nchar(label)>=8 && length(strsplit(label,' ',fixed=TRUE)[[1]])>=2) {
   descriptive <- grepl(label,allcells$normalized,fixed=TRUE)
 }
 candidate <- which(exact | descriptive)
 if(length(candidate)) {
  ev <- allcells[candidate,c('file','sheet','row','column','value','context'),drop=FALSE]
  ev$variable_of_interest <- cd
  ev$match_type <- ifelse(exact[candidate],'EXACT_CODE_TOKEN','DESCRIPTION_CANDIDATE')
  evidence[[length(evidence)+1L]] <- ev[,c('variable_of_interest','match_type','file','sheet','row','column','value','context')]
 }
 summaries[[i]] <- data.frame(input_row=inp$input_row[i],variable_of_interest=cd,name=inp$variable_label[i],
  previous_status=if('available? (Y/N)' %in% names(inp)) clean(inp[['available? (Y/N)']][i]) else '',
  metadata_status=if(any(exact)) 'EXACT_CODE_IN_METADATA' else if(any(descriptive)) 'REVIEW_DESCRIPTION_CANDIDATE' else 'NOT_FOUND_IN_METADATA',
  exact_hits=sum(exact),description_hits=sum(descriptive & !exact),
  candidate_datasets=paste(unique(sub('\\.xls[x]?$','',allcells$file[candidate],ignore.case=TRUE)),collapse='; '),stringsAsFactors=FALSE)
}
summary <- do.call(rbind,summaries)
empty_ev <- data.frame(variable_of_interest=character(),match_type=character(),file=character(),sheet=character(),row=integer(),column=integer(),value=character(),context=character())
ev <- if(length(evidence)) do.call(rbind,evidence) else empty_ev
diag <- if(length(diagnostics)) do.call(rbind,diagnostics) else data.frame()
write.csv(summary,file.path(output_dir,'stardust_gap_summary.csv'),row.names=FALSE,na='')
write.csv(ev,file.path(output_dir,'stardust_metadata_evidence.csv'),row.names=FALSE,na='')
write.csv(diag,file.path(output_dir,'stardust_read_diagnostics.csv'),row.names=FALSE,na='')
cat('Summary rows:',nrow(summary),'| Evidence rows:',nrow(ev),'\n')
print(table(summary$metadata_status))
cat('Saved to:',output_dir,'\n')
cat('CAUTION: Metadata presence does not confirm data collection, populated records or clinical equivalence.\n')
