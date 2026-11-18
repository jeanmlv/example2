#!/usr/bin/env Rscript
# STARDUST metadata gap v4 — input CSV (variable_of_interest,name) OR Excel STARDUST sheet.
# Run from terminal: Rscript /path/stardust_metadata_gap_v4.R /path/input.csv
# Optional arguments: metadata_folder output_folder
suppressPackageStartupMessages(library(readxl))
a <- commandArgs(trailingOnly=TRUE)
if (length(a)<1L) stop('Usage: Rscript stardust_metadata_gap_v4.R input.csv [metadata_dir] [output_dir]')
input_path <- a[1]
metadata_dir <- if(length(a)>=2L) a[2] else '/domino/datasets/local/clinical-trial-data/CNTO1275CRD3005-STARDUST/load-1884/Metadata'
output_dir <- if(length(a)>=3L) a[3] else file.path(metadata_dir,'stardust_metadata_gap_results')
if(!file.exists(input_path)) stop('Input not found: ',input_path)
if(!dir.exists(metadata_dir)) stop('Metadata folder not found: ',metadata_dir)
if(!dir.exists(output_dir) && !dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)) stop('Cannot create output directory')
clean <- function(x) {x <- as.character(x);x[is.na(x)] <- '';trimws(x)}
norm <- function(x) {x <- toupper(clean(x));iconv(x,to='ASCII//TRANSLIT',sub='')}
code_clean <- function(x) {x <- norm(x);x <- gsub('^[^A-Z0-9]+|[^A-Z0-9_]+$','',x);x}
# Accept exported CSV or the original workbook, without dataset specification.
if(grepl('\\.csv$',input_path,ignore.case=TRUE)) {
  inp <- read.csv(input_path,check.names=FALSE,colClasses='character',fileEncoding='UTF-8-BOM',stringsAsFactors=FALSE)
} else if(grepl('\\.xlsx?$',input_path,ignore.case=TRUE)) {
  inp <- as.data.frame(readxl::read_excel(input_path,sheet='STARDUST',col_types='text',.name_repair='unique'),stringsAsFactors=FALSE)
} else stop('Input must be CSV or XLSX')
names(inp) <- trimws(sub('^\\ufeff','',names(inp)))
if(!all(c('variable_of_interest','name') %in% names(inp))) stop('Missing columns variable_of_interest,name. Found: ',paste(names(inp),collapse=', '))
# readxl repairs duplicate 'name' headers; use the first name column.
inp$code <- code_clean(inp[['variable_of_interest']]); inp$label <- clean(inp[['name']]);inp$source_row <- seq_len(nrow(inp))+1L
inp <- inp[nzchar(inp$code) & inp$code!='VARIABLE_OF_INTEREST',,drop=FALSE]
if(nrow(inp)==0L) stop('ZERO input variables: inspect CSV contents, delimiter and variable_of_interest column. No empty reports written.')
cat('Input variables:',nrow(inp),'\n')
files <- sort(list.files(metadata_dir,pattern='\\.xlsx?$',full.names=TRUE,ignore.case=TRUE))
if(!length(files)) stop('No .xls/.xlsx files found in ',metadata_dir)
all <- list(); diag <- list(); n <- 0L
for(f in files) {
  sheets <- tryCatch(readxl::excel_sheets(f),error=function(e) {warning(conditionMessage(e));character()})
  for(sh in sheets) {
    dat <- tryCatch(as.data.frame(readxl::read_excel(f,sheet=sh,col_names=FALSE,col_types='text',.name_repair='minimal',trim_ws=FALSE)),error=function(e) {warning(basename(f),': ',conditionMessage(e));NULL})
    if(is.null(dat)) {diag[[length(diag)+1L]] <- data.frame(file=basename(f),sheet=sh,read_ok=FALSE,cells=0L);next}
    mat <- as.matrix(dat);ix <- which(!is.na(mat) & nzchar(trimws(mat)),arr.ind=TRUE)
    diag[[length(diag)+1L]] <- data.frame(file=basename(f),sheet=sh,read_ok=TRUE,cells=nrow(ix))
    if(!nrow(ix)) next
    n <- n+1L
    all[[n]] <- data.frame(file=basename(f),sheet=sh,row=ix[,1],column=ix[,2],value=as.character(mat[ix]),stringsAsFactors=FALSE)
  }
}
if(!length(all)) stop('No readable nonempty metadata cells. No reports written.')
cells <- do.call(rbind,all);cells$norm <- norm(cells$value)
keys <- paste(cells$file,cells$sheet,cells$row,sep='||')
contexts <- tapply(cells$value,keys,function(v) paste(unique(v),collapse=' | '))
cells$context <- unname(contexts[keys])
# Exact code as separate token, not arbitrary substring.
hit_code <- function(code,v) {
  if(!nzchar(code)) return(rep(FALSE,length(v)))
  vapply(strsplit(v,'[^A-Z0-9_]+',perl=TRUE),function(z) code %in% z,logical(1))
}
hit_label <- function(label,v) {
  q <- norm(label)
  if(nchar(q)<7L || q %in% c('NOT AVAILABLE','N/A','NOT COLLECTED')) return(rep(FALSE,length(v)))
  grepl(q,v,fixed=TRUE)
}
summary_rows <- vector('list',nrow(inp)); evidence <- list()
for(i in seq_len(nrow(inp))) {
  code <- inp$code[i]; label <- inp$label[i]
  exact <- hit_code(code,cells$norm)
  desc <- hit_label(label,cells$norm) & !exact
  ix <- which(exact|desc)
  if(length(ix)) {
    z <- cells[ix,c('file','sheet','row','column','value','context'),drop=FALSE]
    z$variable_of_interest <- code;z$match_type <- ifelse(exact[ix],'EXACT_CODE','DESCRIPTION_CANDIDATE')
    evidence[[length(evidence)+1L]] <- z[,c('variable_of_interest','match_type','file','sheet','row','column','value','context')]
  }
  summary_rows[[i]] <- data.frame(variable_of_interest=code,name=label,
    metadata_status=if(any(exact)) 'CODE_FOUND_IN_METADATA' else if(any(desc)) 'REVIEW_DESCRIPTION' else 'NOT_FOUND_IN_METADATA',
    exact_code_hits=sum(exact),description_hits=sum(desc),
    dataset_candidates=if(length(ix)) paste(unique(sub('\\.xls.*$','',cells$file[ix],ignore.case=TRUE)),collapse='; ') else '',
    interpretation='Metadata evidence only; verify PARAMCD/PARAM and actual ADaM/SDTM data.',stringsAsFactors=FALSE)
}
summary <- do.call(rbind,summary_rows)
if(length(evidence)) ev <- do.call(rbind,evidence) else ev <- data.frame(variable_of_interest=character(),match_type=character(),file=character(),sheet=character(),row=integer(),column=integer(),value=character(),context=character())
write.csv(summary,file.path(output_dir,'stardust_gap_summary.csv'),row.names=FALSE,na='',fileEncoding='UTF-8')
write.csv(ev,file.path(output_dir,'stardust_metadata_evidence.csv'),row.names=FALSE,na='',fileEncoding='UTF-8')
write.csv(do.call(rbind,diag),file.path(output_dir,'stardust_read_diagnostics.csv'),row.names=FALSE,na='')
cat('Metadata files:',length(files),'| Cells:',nrow(cells),'| Summary rows:',nrow(summary),'| Evidence rows:',nrow(ev),'\n')
print(table(summary$metadata_status));cat('Saved in:',output_dir,'\n')
