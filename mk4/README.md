The `cubi_bibliography.bib` is the current master file with all CUBI
publication. This BibTeX file features a number of special fields:

 * `pmid`: the PubMed ID. It is safest to add this manually, because
   automatic search sometimes fail.
 * `lock`: either "all", for all fields, or a comma separated list of
   fields which should never be replaced by PubMed
   records. In some cases, PubMed records are incorrect, messing up names
   or titles.
 * `fulljournalname`: the full unabbreviated journal name from PubMed
 * `cited`: number of citations in google scholar
 * `first`: if the first author is CUBI
 * `last`: if the last author is CUBI
 * `pmcitedin`: number of citations in PubMed (automatically updated)

*TODO*:

 * create a script that fetches a pubmed record based on PMID and prints it
   in BibTeX format
