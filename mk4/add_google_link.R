## generate bibliography in md format using the native R bibentries

library(tidyverse)
library(rentrez)
source("functions.R")

bibfile   <- "cubi_bibliography.bib"
output    <- "cubi_bibliography_with_gs_links.bib"
bib       <- read_bib(bibfile)

for(i in 1:length(bib)) {
  .x <- bib[i]
  bib[i]$google_url <- ""
  link <- "https://scholar.google.com/scholar?q="
  authors <- map_chr(.x$author, ~ {
    family <- .x$family[1]
    paste0("author%3A", family)
  })
  authors <- paste(authors[1:5], collapse = "+")
  links <- paste(link, authors, sep = "")

  year <- sprintf("&as_yhi=%s&as_ylo=%s", .x$year, .x$year)
  links <- paste(links, year, sep = "")

  bib[i]$google_url <- links
}

write_bib_2(bib, output)
