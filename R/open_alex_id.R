# open_alex_id.R
# convert DOI/PMID to openalex ID
# May 2025

open_alex_id = function(indata){
  papers_cited = NULL
  this_doi = filter(indata, !is.na(cited_doi))
  this_pmid = filter(indata, is.na(cited_doi), !is.na(cited_pmid))
  # get the OpenAlex ID for each paper for matching
  works1 = works2 = NULL
  if(nrow(this_doi) > 0){
    dois = filter(indata, !is.na(cited_doi), !str_detect(doi, pattern='figshare|zenodo')) %>% # figshare and zenodo do not return a hit, OSF does
      pull(cited_doi) %>% 
      unique() # remove occasional doubles
    works1 = oa_fetch(entity = 'works', doi = dois)
  }
  if(nrow(this_pmid) > 0){
    pmids = filter(indata, !is.na(cited_pmid)) %>% pull(cited_pmid) %>% unique()
    works2 = oa_fetch(entity = 'works', pmid = pmids)
  }
  if(!is.null(works1) | !is.null(works2)){
    papers_cited = bind_rows(works1, works2) %>%
      select(id, publication_date) %>%
      mutate(id = str_remove(pattern='https://openalex.org/', id))
    papers_cited = unique(papers_cited) # avoid duplicates
  }
  
  return(papers_cited)
}
