# match_papers_reviewers.R
# function to match papers in the reference lists to reviewers, and see if reviewer was cited (key exposure)
# March 2025

match_papers_reviewers = function(reviewers_names,
                                  reviewers_papers, # data frame without excluded list
                                  papers_references_dois,
                                  papers_reviewers_cited,
                                  start = 1,
                                  stop = 2){
  
  # loop through unique combinations of DOIs, version and reviewer

  # big loop, go through every published review
  results = NULL
  for (loop in start:stop){
    
    ## part 1: was the reviewer cited in the paper?
    
    # pull one record
    this_doi = reviewers_names[loop,] %>% pull(doi) # paper DOI
    this_version = reviewers_names[loop,] %>% pull(version)
    this_date = reviewers_names[loop,] %>% pull(date)
    this_review_number = reviewers_names[loop,] %>% pull(review_number) # reviewer number, needed for combining co-reviews
    this_name = reviewers_names[loop,] %>% pull(name) # reviewer's name
    this_role = reviewers_names[loop,] %>% pull(role)
    this_rtext = reviewers_names[loop,] %>% pull(rtext)
    this_country = reviewers_names[loop,] %>% pull(country) # reviewer's country
    this_review_date = reviewers_names[loop,] %>% pull(date) # date of the review
    this_recommendation = reviewers_names[loop,] %>% pull(recommendation) # recommendation
    n_papers_cited_doi_pmid = reviewers_names[loop,] %>% pull(n_papers_cited_doi_pmid) # number of papers the reviewer cited with a DOI or PMID
    
    # get the OpenAlex ID for the papers cited in the paper (those with a DOI or PMID available)
    this_papers_cited = filter(papers_references_dois, doi == this_doi)
    n_papers_cited = nrow(this_papers_cited) # total number of citations (denominator); those with a DOI or PMID
    if(n_papers_cited > 0){
      papers_cited = open_alex_id(indata = this_papers_cited) # convert DOI/PMID to openalex ID
      if(is.null(papers_cited) == TRUE){n_papers_cited = 0} # occasional paper that only cites reports, e.g., 10.12688/openreseurope.19146.2
    }
    
    # get all the reviewers' papers
    this_reviewer_papers = filter(reviewers_papers, doi == this_doi, name == this_name)
    this_reviewer_papers = unique(this_reviewer_papers) # safety net for duplicates
    multiple_match = ifelse(nrow(this_reviewer_papers) == 0, 
                            NA, 
                            this_reviewer_papers$multiple_match[1]) # were there multiple matches for the reviewer's name?
    
    ## citations to the reviewer in the paper
    matches = 0 # dummy for now
    if(nrow(this_reviewer_papers) == 0 | is.null(papers_cited)){ # reviewer has no papers or there are no papers cited
      matches = NA # if nothing to match ...
    }
    if(!is.na(matches)){ # ... otherwise
      matches = length(intersect(papers_cited$id, this_reviewer_papers$id)) # count the matches (ignoring date)
    }
    
    ## use reviewers paper count as a proxy for experience
    # restrict to works published before their review
    works_count = mutate(this_reviewer_papers, 
                         publication_date = as.Date(publication_date, '%Y-%m-%d')) %>%
      filter(publication_date <= this_review_date) %>% 
      nrow()
    
    ## part 2: did reviewer ask for citations to their own paper?
    self_cited_count = 0 # start as zero (no)
    this_reviewer_cited = filter(papers_reviewers_cited, 
                                 doi == this_doi,
                                 name == this_name)
    
    # look for self-citations if any papers cited
    n_reviewer_cited = nrow(this_reviewer_cited)
    if(n_reviewer_cited > 0){
      this_reviewer_papers_cited = open_alex_id(indata = this_reviewer_cited) # convert DOI/PMID to openalex ID
      self_cited_count = sum(this_reviewer_papers_cited$id %in% this_reviewer_papers$id) # count the matches
    }
    
    # finalise the data
    frame = data.frame(doi = this_doi, 
                       version = this_version,
                       date = this_date,
                       name = this_name, 
                       review_number = this_review_number,
                       role = this_role, 
                       recommendation = this_recommendation,
                       rtext = this_rtext,
                       country = this_country,
                       matches, n_papers_cited, multiple_match, works_count, n_reviewer_cited, self_cited_count) # results frame
    # concatenate data
    results = bind_rows(results, frame) #
  
  }
  
  # return
  return(results)
  
}