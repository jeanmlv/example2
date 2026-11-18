#!/usr/bin/env Rscript
# STARDUST ADaM evidence audit v6.2 - metadata + optional real ADaM validation
# Rscript stardust_metadata_gap_v6_2.R [variables.csv|xlsx] [metadata_dir] [output_dir] [adam_data_dir]
# Dependencies: readxl; haven for .xpt/.sas7bdat; arrow for .parquet (optional)
# No patient-level values exported. No claim of 'not collected' from metadata alone.
suppressPackageStartupMessages(library(readxl))
a <- commandArgs(trailingOnly=TRUE)
root <- '/domino/datasets/local/clinical-trial-data/CNTO1275CRD3005-STARDUST/load-1884/Metadata'
arg <- function(i, default) if(length(a)>=i && nzchar(a[[i]])) a[[i]] else default
input_path <- arg(1,file.path(root,'scripts','stardust_variables_of_interest.csv'))
meta_dir <- arg(2,root)
out_dir <- arg(3,file.path(root,'stardust_metadata_gap_results_v6_2'))
data_dir <- arg(4,'')
if(!file.exists(input_path)) stop('Input missing: ',input_path)
if(!dir.exists(meta_dir)) stop('Metadata directory missing: ',meta_dir)
if(nzchar(data_dir) && !dir.exists(data_dir)) stop('ADaM data directory missing: ',data_dir)
text <- function(x) {x<-as.character(x);x[is.na(x)]<-'';x}
code <- function(x) toupper(gsub('[^A-Za-z0-9]','',text(x)))
norm <- function(x) {z<-iconv(text(x),to='ASCII//TRANSLIT',sub='');z[is.na(z)]<-'';toupper(trimws(gsub('[^A-Za-z0-9]+',' ',z)))}
read_input <- function(p) {
 if(grepl('\\.xlsx?$',p,ignore.case=TRUE)) return(as.data.frame(readxl::read_excel(p,sheet='STARDUST',.name_repair='minimal'),check.names=FALSE))
 header<-readLines(p,n=1,warn=FALSE);sep<-if(length(header)&&grepl(';',header,fixed=TRUE)&&!grepl(',',header,fixed=TRUE)) ';' else ','
 read.csv(p,sep=sep,check.names=FALSE,stringsAsFactors=FALSE,fileEncoding='UTF-8-BOM')
}
inp<-read_input(input_path);names(inp)<-tolower(trimws(sub('^\\ufeff','',names(inp))))
if(!all(c('variable_of_interest','name')%in%names(inp))) stop('Input must have variable_of_interest,name')
inp$requested_code<-code(inp$variable_of_interest);inp$requested_name<-text(inp$name)
inp<-inp[nzchar(inp$requested_code),,drop=FALSE];if(!nrow(inp)) stop('No valid input rows')
inp$input_row<-seq_len(nrow(inp))
# Candidate aliases require explicit review; do not silently assert equivalence.
aliases<-list(CALPRO=c('FC'),FC=c('CALPRO'),CDAI=c('CDAITOT'),SESTOT=c('SESCDTOT'),SESCDTOT=c('SESTOT'))
# Evidence is gathered at metadata-cell level, retaining exact workbook coordinates.
meta_files<-list.files(meta_dir,pattern='\\.xlsx?$',full.names=TRUE,ignore.case=TRUE)
if(!length(meta_files)) stop('No metadata workbooks found')
cell_parts<-list();sheet_parts<-list();ci<-0L;si<-0L
for(f in meta_files) {
 sheets<-tryCatch(readxl::excel_sheets(f),error=function(e) character())
 if(!length(sheets)) {si<-si+1L;sheet_parts[[si]]<-data.frame(file=basename(f),sheet='',status='ERROR',rows=0L,cols=0L,detail='Cannot enumerate sheets');next}
 for(sh in sheets) {
  msg<-''
  # Anchor at A1 to preserve workbook coordinates; no arbitrary 5000-row limit.
  tab<-tryCatch(suppressWarnings(readxl::read_excel(f,sheet=sh,range=readxl::cell_limits(c(1,1),c(NA,NA)),col_names=FALSE,col_types='text',.name_repair='minimal')),
     error=function(e){msg<<-conditionMessage(e);NULL})
  si<-si+1L;sheet_parts[[si]]<-data.frame(file=basename(f),sheet=sh,status=if(is.null(tab))'ERROR' else 'OK',rows=if(is.null(tab))0L else nrow(tab),cols=if(is.null(tab))0L else ncol(tab),detail=msg)
  if(is.null(tab)||!nrow(tab)||!ncol(tab)) next
  m<-as.matrix(tab);m[is.na(m)]<-''
  occupied<-matrix(nzchar(trimws(as.vector(m))),nrow=nrow(m),ncol=ncol(m));ij<-which(occupied,arr.ind=TRUE)
  if(!NROW(ij)) next
  # Group by row for compact context; do not export sensitive clinical data (metadata only).
  rr<-unique(ij[,1]);contexts<-setNames(vapply(rr,function(r) substr(paste(m[r,nzchar(trimws(m[r,]))],collapse=' | '),1,1500),character(1)),as.character(rr))
  ci<-ci+1L;cell_parts[[ci]]<-data.frame(file=basename(f),dataset=toupper(sub('\\.xlsx?$','',basename(f),ignore.case=TRUE)),sheet=sh,row=ij[,1],column=ij[,2],value=m[ij],row_context=unname(contexts[as.character(ij[,1])]),stringsAsFactors=FALSE)
 }
}
diag<-do.call(rbind,sheet_parts)
if(!length(cell_parts)) stop('No metadata cells readable')
cells<-do.call(rbind,cell_parts);cells$norm_value<-norm(cells$value)
# Conservative evidence: cell-level exact vs code token inside larger text vs description.
# No fixed column assumed to be a variable-definition column.
evidence_parts<-list();ei<-0L
for(i in seq_len(nrow(inp))) {
 q<-inp$requested_code[i];alias<-aliases[[q]];if(is.null(alias)) alias<-character()
 candidates<-unique(c(q,alias));label<-norm(inp$requested_name[i]);
 # Whole code tokens in text, preserving separators (do not match CDAI inside CDAITOT).
 token_hit<-rep(FALSE,nrow(cells));exact_hit<-rep(FALSE,nrow(cells));alias_hit<-rep(FALSE,nrow(cells))
 for(cand in candidates) {
  pat<-paste0('(^|[^A-Z0-9])',cand,'($|[^A-Z0-9])')
  hit<-grepl(pat,toupper(cells$value),perl=TRUE)
  token_hit<-token_hit|hit
  exact_hit<-exact_hit|(code(cells$value)==cand & nzchar(cells$value))
  if(cand!=q) alias_hit<-alias_hit|hit
 }
 desc_hit<-if(nchar(label)>=12L && grepl(' ',label,fixed=TRUE)) grepl(label,cells$norm_value,fixed=TRUE) else rep(FALSE,nrow(cells))
 ix<-which(token_hit|desc_hit)
 if(length(ix)) {
  e<-cells[ix,c('file','dataset','sheet','row','column','value','row_context'),drop=FALSE]
  e$input_row<-i;e$variable_of_interest<-q
  e$match_type<-ifelse(exact_hit[ix] & !alias_hit[ix],'EXACT_CELL_CODE',ifelse(alias_hit[ix],'ALIAS_CANDIDATE',ifelse(token_hit[ix],'CODE_IN_TEXT','DESCRIPTION_CANDIDATE')))
  e$evidence_role<-ifelse(grepl('CODELIST',toupper(e$sheet)),'CODELIST_CONTEXT',ifelse(grepl('DERIV|PARAMCD|LBTESTCD|IF |THEN ',toupper(e$value)),'DERIVATION_OR_PARAMETER_TEXT','UNVERIFIED_METADATA_CONTEXT'))
  e$metadata_confirmed<-'NO_STRUCTURAL_CONFIRMATION'
  ei<-ei+1L;evidence_parts[[ei]]<-e
 }
}
ev<-if(length(evidence_parts))do.call(rbind,evidence_parts) else data.frame(file=character(),dataset=character(),sheet=character(),row=integer(),column=integer(),value=character(),row_context=character(),input_row=integer(),variable_of_interest=character(),match_type=character(),evidence_role=character(),metadata_confirmed=character())
# Scan actual ADaM files if provided. Use column names + PARAMCD values + nonmissing AVAL/AVALC.
# Unsupported or unreadable files are explicit; no silent negative classifications.
data_files<-if(nzchar(data_dir))list.files(data_dir,pattern='\\.(csv|xpt|sas7bdat|parquet)$',full.names=TRUE,recursive=TRUE,ignore.case=TRUE) else character()
scan_log<-list();data_hits<-list();li<-0L;hi<-0L
for(p in data_files) {
 ext<-tolower(tools::file_ext(p));status<-'OK';detail<-'';d<-NULL
 tryCatch({
  if(ext=='csv') d<-read.csv(p,check.names=FALSE,stringsAsFactors=FALSE)
  else if(ext=='xpt' && requireNamespace('haven',quietly=TRUE)) d<-haven::read_xpt(p)
  else if(ext=='sas7bdat' && requireNamespace('haven',quietly=TRUE)) d<-haven::read_sas(p)
  else if(ext=='parquet' && requireNamespace('arrow',quietly=TRUE)) d<-as.data.frame(arrow::read_parquet(p))
  else {status<-'UNSUPPORTED';detail<-paste('Install reader for',ext)}
 },error=function(e){status<<-'ERROR';detail<<-conditionMessage(e)})
 li<-li+1L;scan_log[[li]]<-data.frame(file=p,status=status,rows=if(is.null(d))NA_integer_ else nrow(d),detail=detail)
 if(is.null(d)) next
 cols<-names(d);norm_cols<-code(cols);dataset<-toupper(sub('\\.[^.]+$','',basename(p)))
 param_cols<-which(norm_cols=='PARAMCD');aval_cols<-which(norm_cols%in%c('AVAL','AVALC'))
 for(i in seq_len(nrow(inp))) {
  q<-inp$requested_code[i];als<-aliases[[q]];if(is.null(als))als<-character()
  for(candidate in unique(c(q,als))) {
   col_ix<-which(norm_cols==candidate)
   if(length(col_ix)) for(j in col_ix) {
    v<-d[[j]];nonmiss<-sum(!is.na(v)&nzchar(trimws(as.character(v))))
    hi<-hi+1L;data_hits[[hi]]<-data.frame(input_row=i,requested_code=q,mapped_code=candidate,dataset=dataset,file=p,match_kind='COLUMN',field=cols[j],matching_records=nrow(d),nonmissing_records=nonmiss,aval_nonmissing=NA_integer_,alias_review=(candidate!=q))
   }
   if(length(param_cols)) for(j in param_cols) {
    matches<-which(!is.na(d[[j]]) & code(d[[j]])==candidate)
    if(length(matches)) {
     aval_nonmiss<-if(length(aval_cols))sum(vapply(aval_cols,function(z) sum(!is.na(d[[z]][matches]) & nzchar(trimws(as.character(d[[z]][matches])))),integer(1))) else NA_integer_
     hi<-hi+1L;data_hits[[hi]]<-data.frame(input_row=i,requested_code=q,mapped_code=candidate,dataset=dataset,file=p,match_kind='PARAMCD',field=cols[j],matching_records=length(matches),nonmissing_records=length(matches),aval_nonmissing=aval_nonmiss,alias_review=(candidate!=q))
    }
   }
  }
 }
 rm(d);invisible(gc(verbose=FALSE))
}
log<-if(length(scan_log))do.call(rbind,scan_log) else data.frame(file=character(),status=character(),rows=integer(),detail=character())
hits<-if(length(data_hits))do.call(rbind,data_hits) else data.frame(input_row=integer(),requested_code=character(),mapped_code=character(),dataset=character(),file=character(),match_kind=character(),field=character(),matching_records=integer(),nonmissing_records=integer(),aval_nonmissing=integer(),alias_review=logical())
complete_meta<-all(diag$status=='OK')
complete_data<-length(data_files)>0L && nrow(log)==length(data_files) && all(log$status=='OK')
summary_parts<-vector('list',nrow(inp))
for(i in seq_len(nrow(inp))) {
 e<-ev[ev$input_row==i,,drop=FALSE];h<-hits[hits$input_row==i,,drop=FALSE]
 direct<-h[!h$alias_review,,drop=FALSE];aliased<-h[h$alias_review,,drop=FALSE]
 direct_populated<-nrow(direct)>0L && any((direct$match_kind=='COLUMN' & direct$nonmissing_records>0L)|(direct$match_kind=='PARAMCD' & (is.na(direct$aval_nonmissing)|direct$aval_nonmissing>0L)))
 direct_defined<-nrow(direct)>0L
 state<-if(direct_populated)'CONFIRMED_AVAILABLE_IN_SCANNED_ADAM' else if(direct_defined)'PRESENT_BUT_NO_CONFIRMED_VALUES' else if(nrow(aliased))'ALIAS_FOUND_REVIEW_REQUIRED' else if(complete_data)'NOT_FOUND_IN_SCANNED_ADAM_SCOPE' else 'NOT_VERIFIED_IN_DATA'
 summary_parts[[i]]<-data.frame(input_row=i,variable_of_interest=inp$requested_code[i],name=inp$requested_name[i],metadata_evidence_count=nrow(e),metadata_candidate_datasets=paste(unique(e$dataset),collapse='; '),metadata_status=if(nrow(e))'REFERENCES_REQUIRE_STRUCTURAL_REVIEW' else if(complete_meta)'NOT_IDENTIFIED_IN_METADATA' else 'INCOMPLETE_METADATA_SCAN',data_status=state,data_files_checked=sum(log$status=='OK'),data_files_total=length(data_files),data_scan_complete=complete_data,matched_datasets=paste(unique(h$dataset),collapse='; '),matched_codes=paste(unique(h$mapped_code),collapse='; '),clinical_collection_confirmed='NOT_TESTED',conclusion=if(state=='CONFIRMED_AVAILABLE_IN_SCANNED_ADAM')'Direct ADaM evidence in scanned files; review field/records' else if(state=='NOT_FOUND_IN_SCANNED_ADAM_SCOPE')'No matching code in scanned ADaM files; does NOT prove not collected' else 'Cannot conclude study-wide availability/absence without validation',stringsAsFactors=FALSE)
}
res<-do.call(rbind,summary_parts)
if(nrow(res)!=nrow(inp)) stop('Summary integrity error')
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
if(!dir.exists(out_dir)) stop('Cannot create output directory')
write.csv(res,file.path(out_dir,'stardust_gap_summary_v6_2.csv'),row.names=FALSE,na='')
write.csv(ev,file.path(out_dir,'stardust_metadata_evidence_v6_2.csv'),row.names=FALSE,na='')
write.csv(diag,file.path(out_dir,'stardust_metadata_read_diagnostics_v6_2.csv'),row.names=FALSE,na='')
write.csv(log,file.path(out_dir,'stardust_adam_data_inventory_v6_2.csv'),row.names=FALSE,na='')
write.csv(hits,file.path(out_dir,'stardust_adam_data_hits_v6_2.csv'),row.names=FALSE,na='')
write.csv(as.data.frame(table(res$data_status)),file.path(out_dir,'stardust_status_counts_v6_2.csv'),row.names=FALSE)
cat('Input variables:',nrow(inp),'\nMetadata files:',length(meta_files),'\nMetadata sheets:',nrow(diag),'\nMetadata evidence:',nrow(ev),'\nADaM files:',length(data_files),'\nData scan complete:',complete_data,'\nOutput:',out_dir,'\n')
print(table(res$data_status))
cat('CAUTION: No finding establishes that a variable was never collected. Alias matches require clinical review.\n')
