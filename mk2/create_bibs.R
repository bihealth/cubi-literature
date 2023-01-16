## generate bibliography in md format using the native R bibentries

library(tidyverse)
library(rentrez)
source("functions.R")

bibfile <- "merged_bibliography_edited_updated.bib"
bib     <- read_bib(bibfile)

year <- "2022"
preprints <- map_lgl(bib, ~ grepl("(arxiv|medrxiv|biorxiv)", .x$journal, ignore.case = T))
sel  <- map_lgl(bib, ~ .x$year == year)

pubs2022 <- map_chr(bib[sel & !preprints], format) %>% paste(collapse="\n\n") %>%
 { gsub(paste0("(", paste0(cubi_authors, collapse="|"), ")( *[[:blank:][:alpha:]]+)", collapse="|"), "**\\1 \\2**", ., ignore.case=TRUE) } %>%
 { gsub("<[^>]*>[,.]", "", .) } %>%
writeLines("publications_2022.md")

#write_bib(bib, "test.bib")

