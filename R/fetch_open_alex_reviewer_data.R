# fetch_open_alex_reviewer_data.R
# get reviewers' papers from Open Alex
# May 2025

# loop through all reviewers
fetch_open_alex_reviewer_data_loop = function(reviewers_names, open_alex_search_results){
  
  # refine search findings
  open_alex_search_results = as.character(open_alex_search_results)
  
  # full list of all files that need to be made
  reviewers_names = mutate(reviewers_names,
                     doi_for_file = str_replace_all(doi, '/', '.'), # only for file
                     outfile = paste(doi_for_file, '_', review_number, '_', role, '.rds', sep=''),
                     index = outfile %in% open_alex_search_results) %>%
    filter(index == FALSE) %>% # only those not yet done
    mutate(runif = runif(n())) %>%
    arrange(runif) %>% # run in random order, better for testing
    select(-doi_for_file, -index, -runif)
  
  # big loop
  start = 1; stop = nrow(reviewers_names)
  for (k in start:stop){
    
    this_reviewer = reviewers_names[k,]
    fetch_open_alex_reviewer_data(this_reviewer)
  
  }
  
}

# function to extract data from OpenAlex for one reviewer
fetch_open_alex_reviewer_data = function(this_reviewer){
  
  excluded = FALSE
  result = NULL
  
  # save results in two folders
  outfile = paste('open_alex_results/', this_reviewer$outfile, sep='') # complex file name for uniqueness
  ex_outfile = paste('open_alex_exclusions/', this_reviewer$outfile, sep='')
  
  ## Find reviewers in OpenAlex
  author = find_author(this_reviewer) # can take a while
  if(suppressWarnings(is.null(author$reason) == FALSE)){ # exclude if no author found in OpenAlex; suppressed warnings
    ex_result = select(author, doi, date, version, review_number, role, name, n_papers_cited_by_reviewer, country, recommendation, reason) # slim down variables
    excluded = TRUE
  }
  
  ## get reviewer's works from OpenAlex
  if(excluded == FALSE){
    alex_id = str_remove(pattern='https://openalex.org/', author$id)
    reviewers_works = oa_fetch(
      entity = "works",
      author.id = alex_id,
      verbose = FALSE) 
    if(is.null(reviewers_works) == TRUE){
      ex_result = mutate(this_reviewer, reason = 'Reviewer has no papers in their OpenAlex record')
      ex_result = select(ex_result, doi, date, version, review_number, role, name, n_papers_cited_by_reviewer, country, recommendation, reason) # slim down variables
      excluded = TRUE
    }
  }
  
  # get the ID numbers of the reviewer's papers
  if(excluded == FALSE){
    
    # papers
    result = mutate(reviewers_works, id = str_remove(id, 'https://openalex.org/')) %>%
      select(id, publication_date) %>%
      mutate(
        multiple_match = author$multiple_match, # needed for later sensitivity analysis
        doi = this_reviewer$doi, # paper DOI for matching with cited papers
        version = this_reviewer$version, # paper version (also for matching)
        role = this_reviewer$role, # needed for co-referees
        review_number = this_reviewer$review_number,
        name = this_reviewer$name,
        rtext = this_reviewer$rtext)

    # only save papers where the review is not excluded
    saveRDS(result, file = outfile) # papers

    # save not excluded reviews too (for bias model)
    ex_result = mutate(this_reviewer, reason = 'Not excluded')
    ex_result = select(ex_result, doi, date, version, review_number, role, name, n_papers_cited_by_reviewer, country, reason) # slim down variables
  }
  
  # save exclusion results (included not excluded too for model that estimates bias)
  saveRDS(ex_result, file = ex_outfile) # excluded results
}
