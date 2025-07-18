# concatenate_reviewers_papers.R
# put together reviewers papers and exclusions from fetch_open_alex_reviewer_data.R
# May 2025

# puts together excluded and not for a data set that can be used to examine exclusions
concatenate_reviewers_exclusions = function(reviewers_names, files){

  # refine files
  files = as.character(files)
  files = files[str_detect(files, '\\.rds$')] # remove any non RDS files
  
  # big loop
  excluded = NULL
  for (file in files){ # loop through all files
    
    result = tryCatch(readRDS(file), error=function(e){'error'}) # with trycatch because of errors
    if(is.character(result)[1] == TRUE){
      cat(file, '\n')
      next
    }
    excluded = bind_rows(excluded, result)
  }
  if(nrow(excluded)!=length(files)){cat('Error, missing files\n')}

  # check that numbers match
  warning = NULL
  data_in = select(reviewers_names, doi, review_number, role) # starting data
  data_out = select(excluded, doi, review_number, role)
  missing = anti_join(data_in, data_out, by = c('doi', 'review_number', 'role'))
  if(nrow(missing) > 0){warning = paste('Not all data included, missing ', nrow(missing), ' rows.', sep='')}
  
  # return
  to_return = list()
  to_return$excluded = excluded
  to_return$warning = warning
  return(to_return)
  
}

# second function to concatenate papers
concatenate_reviewers_papers = function(reviewers_names, files){
  
  # refine files
  files = as.character(files)
  files = files[str_detect(files, '\\.rds$')] # remove any non RDS files
  
  # big loop
  reviewers_papers = NULL
  for (file in files){ # loop through all files
    result = readRDS(file)
    if('reason' %in% names(result)){ # add to exclusions or data
      next
    }
    if(!'reason' %in% names(result)){
      reviewers_papers = bind_rows(reviewers_papers, result)
    }
  }
  
  # check that numbers match
  warning = NULL
  data_in = select(reviewers_names, doi, review_number, role) %>% unique() # starting data
  data_out = select(reviewers_papers, doi, review_number, role) %>% unique() 
  missing = anti_join(data_in, data_out, by = c('doi', 'review_number', 'role'))
  if(nrow(missing) > 0){warning = paste('Not all data included, missing ', nrow(missing), ' rows.', sep='')}
  
  # return
  to_return = list()
  to_return$reviewers_papers = reviewers_papers
  to_return$warning = warning
  return(to_return)
}