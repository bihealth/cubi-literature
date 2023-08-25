stop_words <- c( "about", "again", "all", "almost", "also", "although", "always", "among", "an", "and", "another", "any", "are", "as", "at", "be", "because", "been", "before", "being", "between", "both", "but", "by", "can", "could", "did", "do", "does", "done", "due", "during", "each", "either", "enough", "especially", "etc", "for", "found", "from", "further", "had", "has", "have", "having", "here", "how", "however", "i", "if", "in", "into", "is", "it", "its", "itself", "just", "kg", "km", "made", "mainly", "make", "may", "mg", "might", "ml", "mm", "most", "mostly", "must", "nearly", "neither", "no", "nor", "obtained", "of", "often", "on", "our", "overall", "perhaps", "pmid", "quite", "rather", "really", "regarding", "seem", "seen", "several", "should", "show", "showed", "shown", "shows", "significantly", "since", "so", "some", "such", "than", "that", "the", "their", "theirs", "them", "then", "there", "therefore", "these", "they", "this", "those", "through", "thus", "to", "upon", "various", "very", "was", "we", "were", "what", "when", "which", "while", "with", "within", "without", "would")

#' use Entrez to find a Pubmed record corresponding to the query
find_entrez <- function(b) {
  message("Searching in Pubmed for ", b$key)
  query <- ""

  authors <- map_chr(b$author, ~ .x$family)
  authors <- authors[!grepl("[^[:alnum:].-]", authors)]
  authors <- authors[ !authors %in% c("others", stop_words) ]
  query <- map_chr(authors, ~ paste0(.x, "[AU]")) %>% paste(collapse=" AND ")

  title <- b$title
  title <- gsub("/", "", title)
  title <- gsub("&#[0-9]+;", "", title)
  title <- gsub("\\$[^$]+\\$", "", title)
  title <- gsub("\\{([^{}]*)\\}", "\\1", title)
  title_words <- gsub("[^ [:alnum:]]", " ", title) %>%
    tolower() %>% trimws()
  title_words <- unlist(strsplit(title_words, "  *"))
  title_words <- title_words[ !title_words %in% stop_words ]
  title_words <- title_words[ map_int(title_words, nchar) > 1 ]
  title_words <- paste0(title_words[1:3], "[Ti]") %>% paste(collapse=" AND ")
  query <- paste(query, "AND", title_words)

  if(!is.null(b$year)) {
    query <- paste(query, "AND",
                   sprintf('%s/01/01:3000/12/31[Date - Publication]', b$year))
  }


  ret <- entrez_search(db="pubmed", term=query)
  if(length(ret$ids) != 1) {
    message("ret$ids = ", length(ret$ids), ", retrying")
    ret <- entrez_search(db="pubmed", term=b$title)
  }

  ids <- ret$ids
  if(length(ids) != 1) {
    if(length(ids) == 0) { 
      message("no results!") 
    }
    if(length(ids) > 1) { 
      message("more than one result!")
    } 
    message(format(b))
    print(query)
  }

  #return(ret)
  return(ret$ids)
}

cubi_authors <- read.table(text="
ivanov
bentele
blanc
benary
messerschmidt
obermayer
holtgrewe
zuljan
nieminen
milek
weiner
beule
pett
stolpe
kuhring")[[1]]

is_cubi_last <- function(bibe) {
  return(substr(as.character(tolower(bibe$author[length(bibe$author)]$family) %in% cubi_authors), 1, 1))
}

is_cubi_first <- function(bibe) {
  return(substr(as.character(tolower(bibe$author[1]$family) %in% cubi_authors), 1, 1))
}

#' Format bibliography
format_bib_md <- function(bibs, highlight_authors=NULL, add_url=TRUE) {

  ret <- map_chr(1:length(bibs), ~ format(bibs[[.x]]))
  if(!is.null(highlight_authors)) {
    auth_regex <- paste0(highlight_authors, collapse="|")
    auth_regex <- paste0("(", auth_regex, ")( *[[:blank:][:alnum:]]+[[:alnum:]])", collapse="|")
    ret <- map_chr(ret, ~ gsub(auth_regex, "**\\1 \\2**", ., ignore.case=TRUE))
  }

  ret <- map_chr(ret, ~ gsub("<.*", "", .x))

  if(add_url) {
    ret <- map2_chr(ret, bibs, ~ {
      if(!is.null(.y$url)) {
        .x <- sprintf("%s [☞ Link](%s)", .x, .y$url)
      }
      .x
    })
  }
  
  ret <- ret %>% paste(collapse="\n\n") 
  ret
}




get_authors_style1 <- function(authors) {
  a <- gsub("[^;,[:alnum:]‐–-]", "", authors)
  a <- unlist(strsplit(a, ";"))
  a <- map(a, ~ unlist(strsplit(.x, ",")))
}

get_authors_style2 <- function(authors) {
  a <- authors
  a <- gsub("\\*", "", a)
  a <- unlist(strsplit(authors, ", *"))
  a <- map(a, ~ {
             ret <- unlist(strsplit(.x, "  *"))
             ret <- c(ret[length(ret)], ret[-length(ret)])
          })
}

get_authors_bibtex <- function(authors) {
  a <- authors
  a <- gsub("[[:space:]]", " ", a)
  a <- unlist(strsplit(a, " +and +"))
  a <- map(a, ~ {
             if(grepl(",", .x)) {
              ret <- unlist(strsplit(.x, " *, *"))
             } else if(grepl(" ", .x)) {
              ret <- c(gsub(".* ([^ ]+)$", "\\1", .x),
                       gsub(" [^ ]+$", "", .x))
             } else {
              ret <- .x
             }
             ret
          })
}

get_authors <- function(authors, format=NULL) {
  ll <- list()
  authors <- gsub("[[:space:]]", " ", authors)
  if((grepl(";", authors) && is.null(format)) || 
     (!is.null(format) && format == "semicolon")) {
    return(get_authors_style1(authors))
  }
  if((grepl(" and ", authors) && is.null(format)) || 
     (!is.null(format) && format == "bibtex")) {
    return(get_authors_bibtex(authors))
  }
  ll$a1 <- get_authors_style1(authors)
  ll$a2 <- get_authors_style2(authors)
  ll$a3 <- get_authors_bibtex(authors)
  wm <- which.max(map_int(ll, length))
  return(ll[[wm]])
}

standardize_authors <- function(authors, format=NULL) {
  stopifnot(length(authors) == 1)
  a <- get_authors(authors, format)

  paste(map_chr(a, ~ paste(c(.x[-1], .x[1]), collapse=" ")),
        collapse=" and ")
}

first_word <- function(sentence) {
  sentence <- gsub("^(the|a|in|.) +", "", sentence, ignore.case=TRUE)
  sentence <- gsub("[./]", "", sentence)
  sentence <- gsub("&#[[:alnum:]]+;", "", sentence)
  sentence <- gsub("[^ [:alnum:]]", " ", sentence)
  sentence <- gsub("[^ [:alnum:]]", " ", sentence)
  sentence <- trimws(sentence)
  sentence <- gsub(" .*", "", sentence)
  tolower(sentence)
}

#first_author <- function(authors) {
#  gsub("[^[:alnum:]]", "", gsub("^[A-Za-z][A-Za-z]? ", "", gsub(",.*", "", tolower(authors))))
#}

#' Make a lowercase ascii-only version of a string
clean_author <- function(author) {
  iconv(gsub("[^[:alnum:]]", "", tolower(author)), to="ASCII//TRANSLIT")
}

first_author <- function(authors, format=NULL) {
  clean_author(
  map_chr(authors, ~ 
         get_authors(.x, format)[[1]][1]))
}

make_key <- function(title, year, authors) {
  paste0(first_author(authors), year, first_word(title))
}

#' Read a .bib file returning a list of bibentry objects
#' 
#' @param new_keys Whether new keys should be generated (used to
#'                 standardize the keys)
#' @param standardize Attempt to standardize the fields
read_bib <- function(file, new_keys=TRUE, standardize=TRUE) {
  require(bibtex)
  tmp <- do_read_bib(file)

  ret <- map(tmp, ~ {
        l <- as.list(.x)
        names(l) <- tolower(names(l))
        if(standardize) {
          l$author <- standardize_authors(l$author, format="bibtex")
          l$year   <- gsub(".*([0-9][0-9][0-9][0-9]).*", "\\1", l$year)
        }

        l$bibtype <- attr(.x, "entry")
        l$key     <- attr(.x,  "key")
        
        if(is.null(l$key) || new_keys) {
          l$key <- make_key(l$title, l$year, l$author)
        }
        if(is.null(l$cited))   { l$cited <- "{XXX}" }
        if(is.null(l$journal)) { l$journal <- "unknown" }
        #message(l$key)
        do.call(bibentry, l)
  })

  names(ret) <- map_chr(ret, ~ .x$key)
  ret <- combine_bib(ret)
  message("Read ", length(ret), " entries from file ", file)
  return(ret)
}

is_preprint <- function(b) {
  return(grepl("(arxiv|medrxiv|biorxiv)", b$journal, ignore.case = TRUE))
}

#' convert a list of bibentries into one bibentry object
combine_bib <- function(bib) {
  ret <- bib[[1]]

  if(length(bib) > 1) {
    for(i in 2:length(bib)) {
      ret <- c(ret, bib[[i]])
    }
  }
  return(ret)
}

`%or%` <- function(a, b) {
  if(is.null(a)) return(b)
  else return(a)
}

bib_to_df <- function(bib) {
  imap_dfr(bib, ~ {

          data.frame(authors=paste(format(.x$author), collapse=","),
                     first  = .x$first %or% is_cubi_first(.x),
                     last   = .x$last %or% is_cubi_last(.x),
                     title  = .x$title,
                     year   = .x$year,
                     cited  = as.numeric(.x$cited),
                     journal = .x$journal)
  })
}

authors_stats <- function(authors) {

  


}

#' write bibliography in BibTeX format to a file
write_bib <- function(bib, file) {
  bib_entries <- map_chr(bib, format, "BibTeX") %>% paste(collapse="\n\n")
  writeLines(bib_entries, file)
}


entrez_to_bibentry <- function(pmrec) {
  pmrec$author <- map_chr(pmrec$authors$name, 
                     ~ paste(gsub("^[^[:blank:]]*[[:blank:]]*", "", .x),
                             gsub("^([^[:blank:]]+).*", "\\1", .x))) %>%
             paste(collapse=" and ")
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

  pmrec$pmid <- pmrec$uid

  ret <- do.call(bibentry, pmrec)
}

ask_ok <- function(prompt, default_yes=TRUE) {

  cat(prompt)
  if(default_yes) {
    cat("\nY/n:")
  } else {
    cat("\ny/N:")
  }

  resp <- readline()

  if(resp == "") {
    if(default_yes) { return(TRUE)} else { return(FALSE)}
  }

  if(default_yes) {
    if(substr(tolower(resp), 1, 1) == "n") {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    if(substr(tolower(resp), 1, 1) == "y") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

update_bib_entry_with_pubmed <- function(b, pubmed, pubmed_summaries) {
  stopifnot(!is.null(b$key))

  if(!is.null(b$lock) && b$lock == "all") {
    return(b)
  }

  ## pmid set to NA means the record is not in pubmed (for sure)
  if(!is.null(b$pmid) && b$pmid == "NA") {
    return(b)
  }

  ids <- pubmed[[ b$key ]]
  if(!is.null(b$pmid)) {
    if(!is.null(ids) && !b$pmid %in% ids) {
      stop("The record already has a PMID, but a different one")
    } else {
      ids <- b$pmid
    }
  }

  stopifnot(all(ids %in% names(pubmed_summaries)))

  if(is.null(ids)) {
    message("No pubmed id for ", b$key)
    return(b)
  }

  if(length(ids) == 0) {
    message("No pubmed record found for ", b$key)
    return(b)
  }

  if(length(ids) > 1) {
    cat("==== > Entry:\n")
    cat(format(b), sep = "\n")
    cat("==== > Multiple records. Which matches:\n\n")

    for(i in 1:length(ids)) {
      .id <- ids[i]
      stopifnot(!is.null(pubmed_summaries[[.id]]))
      cat(paste0(i, ". [", .id, "]: "))
      cat(format(entrez_to_bibentry(pubmed_summaries[[.id]])))
      cat("\n\n")
    }
    cat("anything else: none of the above\n")

    sel <- as.integer(readline("Enter your choice:"))

    if(is.na(sel) || !sel %in% 1:length(ids)) {
      cat("OK, aborting\n")
      return(b)
    }

    ids <- ids[sel]
  }

  pmrec <- entrez_to_bibentry(pubmed_summaries[[ids]] )

  mods  <- c()
  empty <- c()
  #if(is.null(b$pmid)) { b$pmid <- ids }

  if(!is.null(b$lock)) {
    lock <- unlist(strsplit(b$lock, " *, *"))
  } else {
    lock <- c()
  }

  if(!setequal(pmrec$author, b$author) && !("author" %in% lock)) {
    mods <- c(mods, "author")
  }
  
  upd_fields <- c("year", "journal", "fulljournalname", "volume", "issue", "pages", "title",
                  "pmid", "url", "doi", "lastauthor", "pmcitedin")
  upd_fields <- setdiff(upd_fields, lock)

  class(b) <- class(pmrec) <- "list"
  for(f in upd_fields) {
    if(!is.null(pmrec[[1]][[f]])) {
      if(is.null(b[[1]][[f]])) {
        #b[[1]][[f]] <- pmrec[[1]][[f]]
        empty <- c(empty, f)
      } else {
        if(b[[1]][[f]] != pmrec[[1]][[f]]) {
          mods <- c(mods, f)
        }
      }
    }
  }
  class(b) <- class(pmrec) <- "bibentry"

  if(length(mods) > 0 || length(empty) > 0) {
    if(length(mods) > 0) {
      cat("\n=> Following fields are different between the two records:\n")
      cat(paste(mods, collapse = ","))
      cat("\n")
    }
    if(length(empty) > 0) {
      cat("=> Following fields are empty in the original record:\n")
      cat(paste(empty, collapse = ","))
      cat("\n")
    }
    cat("\nOrig:\n")
    cat(format(b))
    cat("\nPubmed:\n")
    cat(format(pmrec))
    cat("\n")
    resp <- ask_ok("replace with Pubmed values?")
    if(resp) {
      class(b) <- class(pmrec) <- "list"
      for(f in c(mods, empty)) {
        b[[1]][[f]] <- pmrec[[1]][[f]]
      }
      
      class(b) <- class(pmrec) <- "bibentry"
    }
  }

  return(b)
}


update_bib_with_pubmed <- function(bib, pubmed, pubmed_summaries) {
  ret = list()

  ret <- map(bib, ~ {
               message(.x$key)
               update_bib_entry_with_pubmed(.x, pubmed, pubmed_summaries)
  })

  return(ret)
}

update_keys <- function(bib) {



}






