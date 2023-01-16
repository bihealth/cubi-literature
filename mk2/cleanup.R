# take the merged and edited bibliography file and fill the missing entries
# using PubMed
library(tidyverse)
library(rentrez)
source("functions.R")

bibfile <- "merged_bibliography_edited.bib"
bib     <- read_bib(bibfile)

set_entrez_key("f853f3c25abeb950fa582c5b76e6df613908")

pubmed <- map(bib, ~ {
                if(!is.null(.x$pmid)) {
                  return(.x$pmid)
                } else {
                  return(find_entrez(.x))
                }
             })

pubmed[[32]] <- list()
pubmed_ids       <- unique(unlist(pubmed))  
pubmed_summaries <- entrez_summary(db="pubmed", id = pubmed_ids)

## test update
#bib_upd <- update_bib_with_pubmed(bib[1:5], pubmed, pubmed_summaries)

bib_upd <- update_bib_with_pubmed(bib, pubmed, pubmed_summaries)


# update 31155234 with pubmed manually
pm_sum <- entrez_summary(db="pubmed", id="31155234")
pm_sum <- list(pm_sum)
names(pm_sum) <- "31155234"
bib_upd$heesch2019translational <- update_bib_entry_with_pubmed(bib$heesch2019translational, list(), pm_sum)
bib_upd$heesch2019translational 

write_bib(bib_upd,"merged_bibliography_edited_updated.bib")


