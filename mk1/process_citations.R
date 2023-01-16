library(jsonlite)
library(tidyverse)

foo <- readChar("pubs_found.json_1", file.info("pubs_found.json_1")$size)
pubs1 <- strsplit(gsub("\\} *\\{", "}XxXxX1973{", foo), "XxXxX1973")[[1]]
pubs1 <- map(pubs1, fromJSON)
pubs1 <- map_dfr(pubs1, ~ {
               print(.x$bib$title)
               data.frame(title=.x$bib$title,
                          authors=paste(.x$bib$author, collapse=", "),
                          last="",
                          first="",
                          year=.x$bib$pub_year,
                          journal=.x$bib$venue,
                          cited=.x$num_citations,
                          url=.x$pub_url,
                          google=.x$url_scholarbib)

})


pubs2_l <- readLines("publications.txt")

pubs2 <- list()
curlist <- list()
i <- 1

for(l in pubs2_l) {
  print(l)
  print(length(curlist))
  if(l == "" || grepl("^ *$", l)[1]) {
    next()
  }
  if(grepl("^[a-z]+=[a-z0-9A-Z ]+$", l)) {
    key <- gsub("^([a-z]+)=[a-z0-9A-Z ]+$", "\\1", l)
    val <- gsub("^[a-z]+=([a-z0-9A-Z ]+)$", "\\1", l)
    curlist[[key]] <- val
  } else {
    print("adding")
    if(length(curlist) > 0) { 
      pubs2[[i]] <- curlist 
      i <- i + 1
    }
    curlist <- list( text=l )
  }
  print(paste("after", length(curlist)))
}
#pubs2[[i]] <- curlist


pubs2_b <- map_dfr(pubs2, ~ {
   cur <- .x
   cur$title <- gsub(".*[^*]\\*([^*].*[^*])\\*[^*].*", "\\1", cur$text)
   cur$authors <- gsub("(.*[^*]\\*)([^*].*[^*])\\*[^*].*", "\\1", cur$text)
   cur$cited <- cur$num
   cur$google <- paste0('https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=',
                         URLencode(cur$title))
   cur$url <- ""
   cur$year <- gsub(".*[*][*](.*)(20[12][0-9]) *[*][*].*", "\\2", cur$text)
   cur$journal <- gsub(".*[*][*](.*)(20[12][0-9]) *[*][*].*", "\\1", cur$text)
   cur$num <- NULL
   cur$text <- NULL
   as.data.frame(cur)
})


sel <- c("year", "journal", "first", "last", "cited", "title", "authors", "google", "url")
pubs1 <- pubs1[, sel]
pubs2 <- pubs2_b[, sel]

pubs <- rbind(pubs1, pubs2)

writexl::write_xlsx(pubs, path="processed.xlsx")

pubsE <- readxl::read_excel("processed_edited.xlsx")

jrnl <- readxl::read_excel("JCRSSCIandSCIEwithIF2021.xlsx", skip = 2)

jrn_pat <- set_names(unique(pubsE$journal))
jrn_pat <- gsub("^J\\.", "Journal", jrn_pat)
jrn_pat <- gsub(" J\\.", " Journal", jrn_pat)
jrn_pat <- gsub("[^ a-zA-Z0-9]", "", jrn_pat)

library(stringr)

found <- map(jrn_pat, ~ {
      jj <- .x
      chunks <- strsplit(toupper(jj), " ")[[1]]
      pat <- paste(chunks, collapse=".*")

      return(jrnl[[2]][ which(grepl(pat, jrnl[[2]], ignore.case = TRUE))])
})

found2 <- imap(found, ~ {
                 orig <- .y
                 found <- .x
                 if(length(.x) == 0) {
                   return(.y)
                 } else if(length(.x) == 1) {
                   return(.x)
                 } else {
                   for(i in 1:length(.x)) {
                     cat(sprintf("% 4d %s\n", i, .x[i]))
                   }
                   print(paste("Orig. title: ", .y))
                   inp <- readline(prompt="Enter number or title:")
                   inpI <- as.numeric(inp)
                   if(is.na(inpI)) {
                     return(inp)
                   } else {
                     return(.x[inpI])
                   }
                 }
})

found2["JCO"] <- "JOURNAL OF CLINICAL INVESTIGATION"

pubsE["IF"] <- map_dbl(pubsE$journal, ~ {
                      i_f <- found2[[.x]]
                      i_f <- as.numeric(jrnl[[5]][match(i_f, jrnl[[2]])])
                      print(i_f)
                      if(is.na(i_f)) {
                        i_f <- 0
                      }
                      i_f
})

pubsE <- relocate(pubsE, IF, .before = 6)
writexl::write_xlsx(pubsE, path="CUBI_journal_list_with_citations_2022.xlsx")

