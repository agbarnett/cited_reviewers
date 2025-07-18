# concatenate_xml_data.R
# put the xml data back together
# May 2025

concatenate_xml_data = function(files){
  
  # big loop
  excluded = reviewers_names = ref_meta_data = reference_dois = papers_reviewers_cited = NULL
  for (file in files){
    infile = paste('xml_results/', file, sep='')
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
  
  # make into a list
  to_return = list()
  to_return$ref_meta_data = ref_meta_data # 
  to_return$reviewers_names = reviewers_names 
  to_return$papers_reviewers_cited = papers_reviewers_cited 
  to_return$reference_dois = reference_dois 
  to_return$excluded = excluded
  return(to_return)
}
