## This is a general script supposed to run on a regular basis 
## it is based on cleanup.R

library(tidyverse)
library(rentrez)
source("functions.R")

bibfile <- "cubi_bibliography.bib"
bib     <- read_bib(bibfile, standardize=FALSE, new_keys=FALSE)

set_entrez_key("f853f3c25abeb950fa582c5b76e6df613908")

pubmed <- map(bib, ~ {
                if(!is.null(.x$pmid)) {
                  return(.x$pmid)
                } else if(is_preprint(.x)) {
                  return(list())
                } else {
                  return(find_entrez(.x))
                }
             })
names(pubmed) <- map_chr(bib, ~ .x$key)
pubmed_ids       <- setdiff(unique(unlist(pubmed)), "NA")
pubmed_summaries <- entrez_summary(db="pubmed", id = pubmed_ids)
pubmed_citedin   <- entrez_link(dbfrom="pubmed", id=pubmed_ids, by_id=TRUE) %>%
  map_int(~ length(.x$links$pubmed_pubmed_citedin))
names(pubmed_citedin) <- names(pubmed_summaries)
for(i in names(pubmed_citedin)) {
  pubmed_summaries[[i]]$pmcitedin <- pubmed_citedin[[i]]
}

## test update
#bib_upd <- update_bib_with_pubmed(bib[1:5], pubmed, pubmed_summaries)

bib_upd <- update_bib_with_pubmed(bib, pubmed, pubmed_summaries)

write_bib(bib_upd,"cubi_bibliography_upd.bib")



