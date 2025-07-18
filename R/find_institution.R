## find OpenAlex institution ID from long affiliation text

find_institution = function(text){
  
  # split into each part of the address
  text = str_remove_all(text, "’|'") # Open Alex not a fan of apostrophe's
  strings = str_squish(str_split(text, ',')[[1]])
  strings = unique(strings)
  strings = strings[strings!='']
  strings = strings[nchar(strings)>=3] # remove very short strings, but keep TLAs
  countries = read.csv('data/all_countries.csv') %>% # from https://github.com/agbarnett/ISO-3166-Countries-with-Regional-Codes/tree/master/all
    janitor::clean_names() %>%
    pull(name)
  strings = strings[!strings%in%countries]# remove countries - matches
  
  # return empty result if no strings left
  if(length(strings)==0){
    high = NA
    return(high)
  }
  
  # loop through a search of all parts of the address
  all = NULL
  for (k in 1:length(strings)){
    institution_alex = oa_fetch(
      entity = "institutions",
      search = strings[k],
      verbose = FALSE
    )
    if(is.null(institution_alex)){ # make null frame if no available data
      slim = data.frame(institution_alex = '',
                        id = '',
                        display_name = '',
                        relevance_score = -99, # for sorting
                        k = k)
    }
    if(!is.null(institution_alex)){
      slim = select(institution_alex, id, display_name, relevance_score) %>% mutate(k = k)
    }
    all = bind_rows(all, slim)
  }
  
  # return empty result if no matches
  if(is.null(all)){
    high = NA
    return(high)
  }
  
  # now select match by frequency and score
  if(nrow(all) > 1){
    high = group_by(all, id, display_name) %>% # keep institution ID and display name
      summarise(n = n(), score = sum(relevance_score), .groups = 'drop') %>% # added groups to avoid repeated warning
      arrange(desc(n), desc(score)) %>%
      ungroup() %>%
      slice(1) 
  }
  if(nrow(all)==1){high = all}
  
  #str(high)
  return(high$id) # just return the openalex id
}

