## here we create a crude BibTeX file based on the Excel file stored in
## October 2022. This BibTeX file will then be later amended with the
## newest entries.

## then, add any bibliography entries from sources/*.bib

library(readxl)
library(tidyverse)
library(bibtex)
library(stringr)
source("functions.R")


## =====================================================================================
## part 1: reading the excel file

xfile <- "processed_edited.xlsx"
pubs <- read_excel(xfile)

pubs <- pubs %>% rowwise() %>% 
  mutate(key=make_key(title, year, authors)) %>%
  mutate(bibentry=list(bibentry(key=key, 
                           bibtype="article",
                           title=title,
                           author=standardize_authors(authors),
                           year=year,
                           url=url,
                           first=first,
                           last=last,
                           cited=cited,
                           journal=journal)))

pubs_bib <- pubs$bibentry
names(pubs_bib) <- pubs$key

## =====================================================================================
## part 2: reading the bib files

bibfiles <- list.files("sources/", pattern = "*.bib", ignore.case = TRUE,
                       full.names = TRUE)
refs_from_bibs <- unlist(map(bibfiles, read_bib), recursive=FALSE)

dups <- duplicated(names(refs_from_bibs))
if(any(dups)) {
  warning(sprintf("Duplicate entries found when reading bib files (%d):\n%s",
                  sum(dups),
                  paste(names(refs_from_bibs)[dups],
                        collapse=",")
                       ))
  refs_from_bibs <- refs_from_bibs[ !duplicated(names(refs_from_bibs)) ]
}

dups <- names(refs_from_bibs) %in% names(pubs_bib)
if(any(dups)) {
  warning(sprintf(
    "Following entries from file %s will be replaced by entries from BibTeX files:\n%s",
    xfile,
    paste(names(refs_from_bibs)[dups], collapse=",")))
}

for(k in names(refs_from_bibs)) {
  pubs_bib[[k]] <- refs_from_bibs[[k]]
}

## =====================================================================================
## part 3. some cleanup and adding missing entries.




## =====================================================================================
## part 4. saving the bib entries as a bibtex file

years <- map_int(pubs_bib, ~ as.integer(.x$year))
ord <- order(-years)
pubs_bib <- pubs_bib[ord]

bib_entries <- map_chr(pubs_bib, format, "BibTeX") %>% paste(collapse="\n\n")
writeLines(bib_entries, "merged_bibliography.bib")

message("First, you should check the entries against PUBMED, but this is not going to happen.
Now go and edit merged_bibliography.bib")

