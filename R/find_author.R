# find_author.R
## function to find author/reviewer using multiple attempts
## called by fetch_open_alex_reviewer_data.R

find_author = function(inauthor, author_checks = FALSE){
  
  # attempt 1: if there's an ORCID
  if(is.na(inauthor$orcid) == FALSE){
    author <- oa_fetch(
      entity = "authors",
      orcid = inauthor$orcid # includes https://orcid.org/
    )
    # note, cannot use other functions, e.g., rorcid, because we need the open alex ID
    if(is.null(author) == FALSE){ # return results if there's a record
      author = mutate(author,
                      from_orcid = TRUE,
                      multiple_match = FALSE,
                      mtype = 1)
      return(author) # stop here (key variable is author$id)
    }
  }
  
  # attempt 2: if there's no ORCID, search using reviewer's name 
  reviewer_name = with(inauthor, paste(first_name, surname))
  author <- oa_fetch( # can be very slow for common names
    entity = "authors",
    display_name.search = reviewer_name
  )
  # fix for occasionally massive number of authors
  if(!is.null(nrow(author))){
    author = arrange(author, desc(relevance_score)) %>%
      slice(1:10) # take top ten
  }
  #
  if(is.null(nrow(author))){ # no Open Alex record then try ...
    # a) add spaces after initials
    updated_reviewer_name = str_squish(str_replace_all(reviewer_name, pattern="\\.", replacement = '\\. ')) # add spaces after initials
    if(updated_reviewer_name != reviewer_name){ # only re-run if there's a name difference
      author <- oa_fetch(
        entity = "authors",
        display_name.search = updated_reviewer_name
      ) 
    }
    if(is.null(nrow(author))){ # no Open Alex record then try ...
      # b) without middle name/initial
      updated_reviewer_name = str_squish(str_replace_all(reviewer_name, ' [A-Z|a-z]\\.? ', ' '))
      if(updated_reviewer_name != reviewer_name){ # only re-run if there's a name difference
        author <- oa_fetch(
          entity = "authors",
          display_name.search = updated_reviewer_name
        ) 
      }
    }
    if(is.null(nrow(author))){ # if still null then exclude
      inauthor$reason = 'No Open Alex record found'
      return(inauthor) # stop here, return reviewer not author if excluded
    }
  }
  
  # multiple potential matches?
  multiple_match = ifelse(nrow(author)>1, TRUE, FALSE) # multiple matches for the same author
  
  if(multiple_match == FALSE){ # stop here if just one match
    author = mutate(author,
                    from_orcid = FALSE,
                    multiple_match = FALSE,
                    mtype = 2) # 
    return(author) # stop here (key variable is author$id)
  }
  
  ## keep if exact name match as top-ranked match
  if(author$display_name[1] == reviewer_name){
    author = mutate(author[1,], # just first row
                    from_orcid = FALSE,
                    multiple_match = TRUE,
                    mtype = 3) # 
    return(author) # stop here (key variable is author$id)
  }
  
  ## attempt 3: match on affiliation if more than one match; use open alex institution ID
  # exclude missing affiliations
  if(is.null(author$affiliation_display_name)){ # this variable is not returned for some records
    author = mutate(author, affiliation_display_name = NA)
  }
  author = filter(author, !is.na(affiliation_display_name))
  if(nrow(author)==0){
    inauthor$reason = 'No Open Alex record found'
    return(inauthor) # stop here, return reviewer not author if excluded
  }
  author$institution_id = NA
  for (b in 1:nrow(author)){
    author$institution_id[b] = find_institution(author$affiliation_display_name[b])
  }
  # can take a while ... why?
  inauthor = mutate(inauthor, institution_id = find_institution(affiliation)) # add affiliation to reviewer
  # match by institution ID
  match1 = filter(author, institution_id == inauthor$institution_id)
  if(nrow(match1) > 0){ # stop here if match
    # randomly export some for checking
    rand = runif(n = 1)
    if(rand < 0.05 & author_checks == TRUE){export_to_check_file(inauthor, match1[1,])}
    #
    author = match1[1,] # rename and take only top row
    author = mutate(author,
                    from_orcid = FALSE,
                    multiple_match = TRUE,
                    mtype = 4)
    return(author) # stop here (key variable is author$id)
  }
  
  # could look at affiliations_other for previous institutions
  
  ## if no matching from OpenAlex then re-match based on number of words matching in affiliation
  # first match on country
  author_country = filter(author, affiliation_country_code == inauthor$country_code)
  if(nrow(author_country)==0){
    inauthor$reason = 'No Open Alex record found'
    return(inauthor) # stop here, return reviewer not author if excluded
  }
  author_country$p_match = NA
  for (j in 1:nrow(author_country)){
    author_country$p_match[j] = match_words(few_words = author_country$affiliation_display_name[j], to_text_match = inauthor$affiliation)
  }
  match1 = filter(author_country, p_match > 0.5) %>% # threshold for matching set at over a half
    arrange(desc(p_match)) %>%
    slice(1) # take best match
  if(nrow(match1) == 0){ 
    inauthor$reason = 'No Open Alex record found'
    return(inauthor) # stop here, return reviewer not author if excluded
  }
  author = match1 # over-write
  
  # end
  author = author[1,] # safety net in case there are still multiple matches
  author = mutate(author,
                  from_orcid = FALSE,
                  multiple_match = TRUE,
                  mtype = 5)
  return(author) #  (key variable is author$id)
}
