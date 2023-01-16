## generate bibliography in md format using the native R bibentries

library(tidyverse)
library(rentrez)
source("functions.R")

bibfile <- "cubi_bibliography.bib"
bib     <- read_bib(bibfile)

preprints <- map_lgl(bib, is_preprint)

years <- map_chr(bib, ~ .x$year) %>% unique()

for(year in years){
  sel  <- map_lgl(bib, ~ .x$year == year)
  pubs <- format_bib_md(bib[ sel & !preprints ], highlight_authors=cubi_authors)
  writeLines(pubs, sprintf("publications_%s.md", year))
}

pubs <- format_bib_md(bib[preprints], highlight_authors=cubi_authors)
writeLines(pubs, "publications_preprints.md")


#write_bib(bib, "test.bib")

