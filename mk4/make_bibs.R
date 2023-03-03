## generate bibliography in md format using the native R bibentries

library(tidyverse)
library(rentrez)
source("functions.R")

output_dir <- "md"
dir.create(output_dir, showWarnings = FALSE)

bibfile   <- "cubi_bibliography.bib"
bib       <- read_bib(bibfile)
preprints <- map_lgl(bib, is_preprint)
years     <- map_chr(bib, ~ .x$year) %>% unique()

if(file.exists(file.path(output_dir, "publications.md"))) {
  file.remove(file.path(output_dir, "publications.md"))
}
full_bib  <- file(file.path(output_dir, "publications.md"), "a")

pubs <- format_bib_md(bib[preprints], highlight_authors=cubi_authors)
writeLines(pubs, file.path(output_dir, "publications_preprints.md"))


writeLines("### Preprints\n\n", full_bib)
writeLines(pubs, full_bib)

for(year in sort(as.integer(years), decreasing=TRUE)){
  sel  <- map_lgl(bib, ~ .x$year == year)
  pubs <- format_bib_md(bib[ sel & !preprints ], highlight_authors=cubi_authors)
  writeLines(pubs, file.path(output_dir, sprintf("publications_%s.md", year)))
  writeLines(sprintf("\n\n### %s\n\n", year), full_bib)
  writeLines(pubs, full_bib)
}


#write_bib(bib, "test.bib")

close(full_bib)

