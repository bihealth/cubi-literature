#!/usr/bin/env Rscript
## fetch a pubmed ID and print it in BibTeX format

args = commandArgs(trailingOnly=TRUE)

id <- args[1]
if(is.na(id)) {
  stop("usage: fetch_pm pubmed_id")
}


library(purrr)
library(rentrez)

entrez_to_bibentry <- function(pmrec, key=NULL) {
  if(!is.null(key)) { pmrec$key <- key }
  pmrec$author <- map_chr(pmrec$authors$name, 
                     ~ paste(gsub("^[^[:blank:]]*[[:blank:]]*", "", .x),
                             gsub("^([^[:blank:]]+).*", "\\1", .x))) 
  pmrec$author <- paste(pmrec$author, collapse=" and ")
  if(!is.null(pmrec$pubdate) && pmrec$pubdate != "") {
    pmrec$year <- gsub(" .*", "", pmrec$pubdate)
  } else if(!is.null(pmrec$epubdate) && pmrec$epubdate != "") {
    pmrec$year <- gsub(" .*", "", pmrec$epubdate)
  } else {
    pmrec$year <- '0000'
  }
  pmrec$bibtype <- "article"
  pmrec$journal <- pmrec$source

  if("doi" %in% pmrec$articleids$idtype) {
    pmrec$doi <- pmrec$articleids$value[ match("doi", pmrec$articleids$idtype) ]
  }

  if(grepl("^http", pmrec$elocationid)) {
    pmrec$url <- pmrec$elocationid
  } else if(!is.null(pmrec$doi)) {
    pmrec$url <- paste0("https://doi.org/", pmrec$doi)
  }
  pmrec$authors <- 
    pmrec$articleids <- 
    pmrec$history <- 
    pmrec$references <- NULL

  pmrec$pmid <- pmrec$uid

  ret <- do.call(bibentry, pmrec)
}

autokey <- function(pmrec) {
  tit <- tolower(pmrec$title)
  tit <- gsub("[^[:blank:][:alnum:]]*", "", tit)
  stop_words <- c( "about", "again", "all", "almost", "also", "although", "always", "among", "an", "and", "another", "any", "are", "as", "at", "be", "because", "been", "before", "being", "between", "both", "but", "by", "can", "could", "did", "do", "does", "done", "due", "during", "each", "either", "enough", "especially", "etc", "for", "found", "from", "further", "had", "has", "have", "having", "here", "how", "however", "i", "if", "in", "into", "is", "it", "its", "itself", "just", "kg", "km", "made", "mainly", "make", "may", "mg", "might", "ml", "mm", "most", "mostly", "must", "nearly", "neither", "no", "nor", "obtained", "of", "often", "on", "our", "overall", "perhaps", "pmid", "quite", "rather", "really", "regarding", "seem", "seen", "several", "should", "show", "showed", "shown", "shows", "significantly", "since", "so", "some", "such", "than", "that", "the", "their", "theirs", "them", "then", "there", "therefore", "these", "they", "this", "those", "through", "thus", "to", "upon", "various", "very", "was", "we", "were", "what", "when", "which", "while", "with", "within", "without", "would")
  stop_words <- paste0("\\<(", paste0(stop_words, collapse="|"), ")\\>")
  tit <- gsub(stop_words, "", tit)
  tit <- gsub("\\<[[:alnum:]]\\>", "", tit)
  tit <- gsub(" .*", "", tit)
  autname <- tolower(gsub(" .*", "", pmrec$authors$name[1]))
  year <- gsub(".*([0-9][0-9][0-9][0-9]).*", "\\1", pmrec$pubdate)
  return(paste0(autname, year, tit))
}

pub <- entrez_summary(db="pubmed", id=id, by_id=TRUE)
entry <- entrez_to_bibentry(pub, autokey(pub))

cat(format(entry, "BibTeX"))
cat("\n\n")
