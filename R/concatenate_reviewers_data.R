# concatenate_reviewers_data.R
# put the reviewers' papers data back together
# Jan 2025

concatenate_reviewers_data = function(papers, query_result){
  
  # big loop
  excluded = reviewers_papers = NULL
  for (pmid in query_result$ids){
    infile = paste('xml_results/', pmid, '.rds', sep='')
    result = readRDS(infile) 
    # some are exclusions...
    if(class(result) == 'data.frame'){
      excluded = bind_rows(excluded, result)
    }
    # ... others have data
    if(class(result) != 'data.frame'){
      reviewers_names = bind_rows(reviewers_names, result$reviewers)
      ref_meta_data = bind_rows(ref_meta_data, result$ref_meta_data)
      reference_dois = bind_rows(reference_dois, result$papers_references_dois)
      papers_reviewers_cited = bind_rows(papers_reviewers_cited, result$papers_reviewers_cited)
    }
  }
  
  # check numbers
  if(nrow(papers) != query_result$count){stop("Error, numbers do not match\n")}
  
  # make into a list
  to_return = list()
  to_return$ref_meta_data = ref_meta_data # 
  to_return$excluded = excluded
  return(to_return)
}
