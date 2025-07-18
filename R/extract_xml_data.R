# extract_xml_data.R
# loop to extract data from the journals' web sites using the XML version of the paper
# get: 1) reviewers' names, 2) paper's reference lists, 3) exclusions, 4) papers the reviewers cited, 5) meta-data
# cannot use OpenAlex for paper's reference lists as it includes papers cited by the reviewer
# save each paper to an individual file
# May 2025

# function to loop through DOIs
loop_extract_xml_data = function(all_dois, xml_search_results){
  
  #
  N = nrow(all_dois)
  all_dois = sample_n(all_dois, size = N, replace = FALSE) # randomly re-order to find bugs faster
  start = 1 
  stop = N 
   
  # refine search findings
  xml_search_results = as.character(xml_search_results)
  xml_search_results = str_remove_all(xml_search_results, pattern='xml_results/')
  
  # big loop of papers:
  for (index in start:stop) { 

    #    
    this_doi = all_dois$doi[index]
    this_doi_file = str_replace_all(this_doi, '/', '_')
    
    # check if result for this DOI already exists
    outfile = paste(this_doi_file, '.rds', sep='')
    exists = outfile %in% xml_search_results
    if(exists == TRUE){
      next # move on to next paper
    }
    
    # run one paper:
    extract_xml_data(doi = this_doi) 
  }
}

# function to run one paper
extract_xml_data = function(doi) {

  # save results in this file, use DOI for file naming
  doi_file = str_replace_all(doi, '/', '_')
  outfile = paste('xml_results/', doi_file, '.rds', sep='')
  
  # function that extracts reviewers names and other details:
  result = extract_details(doi) 
  
  # exclusions
  if (is.null(result$excluded) == FALSE) {
    result = data.frame(doi = doi, reason = result$excluded_reason)
  }
  if (is.null(result$excluded) == TRUE & is.null(result$reviewers) == TRUE) { # slight mistake here as this is over-riding reason above, now fixed with is.null(result$excluded) == TRUE but not run
    result = data.frame(doi = doi, reason = 'No reviewers')
  }

  # save list of results to external file
  saveRDS(result, file = outfile)
  
}
