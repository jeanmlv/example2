# example2

#!/usr/bin/env Rscript
# STARDUST ADaM metadata evidence audit v6
# Usage: Rscript stardust_metadata_gap_v6.R [input.csv|input.xlsx] [metadata_dir] [output_dir]
# Requires: readxl. Read-only source processing; no clinical data exported.
suppressPackageStartupMessages(library(readxl))
args <- commandArgs(trailingOnly=TRUE)
root <- '/domino/datasets/local/clinical-trial-data/CNTO1275CRD3005-STARDUST/load-1884/Metadata'
input_path <- if(length(args)>0) args[1] else file.path(root,'scripts','stardust_variables_of_interest.csv')
meta_dir <- if(length(args)>1) args[2] else root
out_dir <- if(length(args)>2) args[3] else file.path(root,'stardust_metadata_gap_results_v6')
if(!file.exists(input_path)) stop('Input not found: ',input_path)
if(!dir.exists(meta_dir)) stop('Metadata directory not found: ',meta_dir)
clean <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- toupper(trimws(x))
  gsub("[^A-Z0-9]+", "", x)
}
normalize <- function(x) {x<-iconv(clean(x),to='ASCII//TRANSLIT',sub='');x[is.na(x)]<-'';toupper(trimws(gsub('[^A-Za-z0-9]+',' ',x)))}
code <- function(x) toupper(gsub('[^A-Za-z0-9_]','',clean(x)))
read_input <- function(path) {
 if(grepl('\\.xlsx?$',path,ignore.case=TRUE)) return(as.data.frame(readxl::read_excel(path,sheet='STARDUST',.name_repair='minimal'),check.names=FALSE))
 header <- readLines(path,n=1,warn=FALSE,encoding='UTF-8')
 sep <- if(length(header) && grepl(';',header,fixed=TRUE) && !grepl(',',header,fixed=TRUE)) ';' else ','
 read.csv(path,sep=sep,check.names=FALSE,stringsAsFactors=FALSE,fileEncoding='UTF-8-BOM')
}
inp <- read_input(input_path)
names(inp) <- tolower(trimws(sub('^\\ufeff','',names(inp))))
required <- c('variable_of_interest','name')
if(!all(required %in% names(inp))) stop('Input requires variable_of_interest and name. Found: ',paste(names(inp),collapse=', '))
inp$variable_code <- code(inp$variable_of_interest)
inp$variable_label <- clean(inp$name)
inp <- inp[nzchar(inp$variable_code),,drop=FALSE]
if(!nrow(inp)) stop('No valid variables in input')
inp$input_row <- seq_len(nrow(inp))
files <- list.files(meta_dir,pattern='\\.xlsx?$',full.names=TRUE,ignore.case=TRUE)
if(!length(files)) stop('No XLS/XLSX files in metadata directory')
# Preserve Excel coordinates; readxl trims leading blank rows/columns by default.
# Explicit wide range anchors A1 and supports up to 2000 rows and 60 columns.
# Warnings are logged; failed sheets cause an INCOMPLETE audit status.
all <- list(); diagnostics <- list(); k <- 0L
for(f in files) {
 sheets <- tryCatch(readxl::excel_sheets(f),error=function(e) {warning(conditionMessage(e));character()})
 if(!length(sheets)) diagnostics[[length(diagnostics)+1L]] <- data.frame(file=basename(f),sheet='',status='ERROR',message='No readable sheets')
 for(s in sheets) {
  err <- ''
  tab <- tryCatch(suppressWarnings(readxl::read_excel(f,sheet=s,range=readxl::cell_limits(c(1,1),c(2000,60)),col_names=FALSE,col_types='text',.name_repair='minimal')),error=function(e) {err<<-conditionMessage(e);NULL})
  diagnostics[[length(diagnostics)+1L]] <- data.frame(file=basename(f),sheet=s,status=if(is.null(tab)) 'ERROR' else 'OK',message=err)
  if(is.null(tab)) next
  mat <- as.matrix(tab)
  if(!length(mat)) next
  mat[is.na(mat)] <- ''
  inds <- which(nzchar(trimws(mat)),arr.ind=TRUE)
  if(!nrow(inds)) next
  for(j in seq_len(nrow(inds))) {
   r <- inds[j,1]; c <- inds[j,2]; val <- clean(mat[r,c]);
   ctx <- paste(clean(mat[r,which(nzchar(trimws(mat[r,])))]),collapse=' | ')
   k<-k+1L
   all[[k]] <- data.frame(file=basename(f),dataset=toupper(sub('\\.xlsx?$','',basename(f),ignore.case=TRUE)),sheet=s,row=r,column=c,value=val,row_context=substr(ctx,1,3000),stringsAsFactors=FALSE)
  }
 }
}
if(!length(all)) stop('No metadata cells could be read. No reports created.')
cells <- do.call(rbind,all)
diag <- do.call(rbind,diagnostics)
# Structural classification: column 7 is variable name in ADaM dataset tabs,
# column 8 in ADM Codelist is often the code and column 10 its label.
# These positions are based on observed ADEFF/ADQS exports; each row is retained for review.
classify <- function(sheet,col,val,query_code) {
 s<-toupper(trimws(sheet));v<-code(val)
 if(s=='ADM CODELIST' && col==8 && v==query_code) return('CODELIST_CODE')
 if(s=='ADM CODELIST' && col==10) return('CODELIST_LABEL')
 if(s=='ADM DATASET') return('DATASET_STRUCTURE_REFERENCE')
 if(s %in% c('ADEFF','ADQS','ADSL','ADAE','ADCM','ADLB','ADMH','ADPC','ADVS','ADTE','ADTFTE','ADDDV','ADDX','ADDISP','ADLBS') && col==7 && v==query_code) return('DEFINED_ADAM_VARIABLE')
 if(col==21 || grepl('\\b(IF|THEN|ELSE|DERIV|SET TO|PARAMCD|LBTESTCD)\\b',toupper(val),perl=TRUE)) return('DERIVATION_REFERENCE')
 'OTHER_TEXT_REFERENCE'
}
# Clinical synonyms are search candidates only, not confirmed equivalence.
synonyms <- list(
 CALPRO=c('FECAL CALPROTECTIN','FAECAL CALPROTECTIN','CALPROTECTIN'),
 CRP=c('C REACTIVE PROTEIN'),
 SESCDTOT=c('SES CD TOTAL','SIMPLE ENDOSCOPIC SCORE','SESCD TOTAL'),
 CDAI=c('CROHN S DISEASE ACTIVITY INDEX','CDAI TOTAL'),
 CDAITOT=c('CROHN S DISEASE ACTIVITY INDEX'),
 FC=c('FECAL CALPROTECTIN','FAECAL CALPROTECTIN')
)
# Prevent broad labels like AGE, SEX, N, etc. from creating fuzzy hits.
evidence <- list(); summary <- list()
for(i in seq_len(nrow(inp))) {
 q<-inp$variable_code[i]; label<-normalize(inp$variable_label[i]);
 # Code must be a full token; never match substrings of longer codes.
 pattern<-paste0('(^|[^A-Z0-9_])',q,'($|[^A-Z0-9_])')
 exact<-grepl(pattern,toupper(cells$value),perl=TRUE)
 terms<-unique(c(if(nchar(label)>=12 && length(strsplit(label,' ',fixed=TRUE)[[1]])>=2) label else character(),synonyms[[q]]))
 desc<-rep(FALSE,nrow(cells))
 for(term in terms) if(nchar(term)>=8) desc<-desc | grepl(term,normalize(cells$value),fixed=TRUE)
 idx<-which(exact|desc)
 ev <- if(length(idx)) cells[idx,,drop=FALSE] else NULL
 if(!is.null(ev)) {
  ev$variable_of_interest<-q
  ev$match_basis<-ifelse(exact[idx],'EXACT_CODE_TOKEN','DESCRIPTION_CANDIDATE')
  ev$evidence_type<-mapply(function(sh,co,v,ex) if(!ex) 'CLINICAL_CONCEPT_CANDIDATE' else classify(sh,co,v,q),ev$sheet,ev$column,ev$value,exact[idx],USE.NAMES=FALSE)
  ev$evidence_type[ev$evidence_type=='OTHER_TEXT_REFERENCE' & ev$match_basis=='EXACT_CODE_TOKEN'] <- 'OTHER_TEXT_REFERENCE'
  ev$review_required <- TRUE
  evidence[[length(evidence)+1L]]<-ev[,c('variable_of_interest','dataset','file','sheet','row','column','match_basis','evidence_type','value','row_context','review_required')]
 }
 types<-if(is.null(ev)) character() else unique(ev$evidence_type)
 level<-if('DEFINED_ADAM_VARIABLE' %in% types) 'DEFINED_ADAM_VARIABLE' else if('CODELIST_CODE' %in% types) 'CODELIST_CODE_DOCUMENTED' else if('DERIVATION_REFERENCE' %in% types) 'DERIVATION_REFERENCE_ONLY' else if('CLINICAL_CONCEPT_CANDIDATE' %in% types || 'CODELIST_LABEL' %in% types) 'CLINICAL_CONCEPT_CANDIDATE' else if(length(types)) 'OTHER_METADATA_REFERENCE' else 'NOT_IDENTIFIED_IN_METADATA'
 incomplete<-any(diag$status!='OK')
 if(incomplete && level=='NOT_IDENTIFIED_IN_METADATA') level<-'INCOMPLETE_METADATA_REVIEW'
 summary[[i]]<-data.frame(input_row=inp$input_row[i],variable_of_interest=q,name=inp$variable_label[i],previous_status=if('available? (y/n)' %in% names(inp)) clean(inp[['available? (y/n)']][i]) else '',evidence_level=level,metadata_documented=if(level=='DEFINED_ADAM_VARIABLE') 'YES_VARIABLE_DEFINITION' else if(level=='CODELIST_CODE_DOCUMENTED') 'YES_CODELIST_REFERENCE' else if(level=='NOT_IDENTIFIED_IN_METADATA') 'NO_EVIDENCE_FOUND' else 'REVIEW_REQUIRED',dataset_presence_confirmed='NOT_TESTED',clinical_collection_confirmed='NOT_TESTED',evidence_count=length(idx),candidate_datasets=if(length(idx)) paste(unique(ev$dataset),collapse='; ') else '',review_recommendation=if(level=='NOT_IDENTIFIED_IN_METADATA') 'Review SDTM and actual ADaM datasets before declaring unavailable' else 'Validate evidence context and actual dataset/PARAMCD values',stringsAsFactors=FALSE)
}
res<-do.call(rbind,summary)
ev<-if(length(evidence)) do.call(rbind,evidence) else data.frame(variable_of_interest=character(),dataset=character(),file=character(),sheet=character(),row=integer(),column=integer(),match_basis=character(),evidence_type=character(),value=character(),row_context=character(),review_required=logical())
if(nrow(res)!=nrow(inp)) stop('INTERNAL ERROR: summary count differs from input; no files exported')
# Exports are atomic-ish: create output only after successful validation.
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
if(!dir.exists(out_dir)) stop('Cannot create output directory: ',out_dir)
write.csv(res,file.path(out_dir,'stardust_gap_summary_v6.csv'),row.names=FALSE,na='')
write.csv(ev,file.path(out_dir,'stardust_metadata_evidence_v6.csv'),row.names=FALSE,na='')
write.csv(diag,file.path(out_dir,'stardust_read_diagnostics_v6.csv'),row.names=FALSE,na='')
write.csv(as.data.frame(table(res$evidence_level),stringsAsFactors=FALSE),file.path(out_dir,'stardust_status_counts_v6.csv'),row.names=FALSE)
cat('Input variables:',nrow(inp),'\nMetadata files:',length(files),'\nMetadata cells:',nrow(cells),'\nSummary rows:',nrow(res),'\nEvidence rows:',nrow(ev),'\nRead errors:',sum(diag$status!='OK'),'\nOutput:',out_dir,'\n')
print(table(res$evidence_level))
cat('IMPORTANT: Metadata evidence does NOT prove records exist or were collected.\n')

