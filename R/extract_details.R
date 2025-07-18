# extract_details.R
# function to extract reviewers' names and details from the XML 
# called by extract_xml_data.R
# June 2025

extract_details = function(doi){
  
  # regex patterns used below
  after_comma = ',.*'
  
  ## first get journal from DOI
  this_journal = case_when(
    str_detect(doi, pattern='f1000') ~ 'F1000Research',
    str_detect(doi, pattern='wellcome') ~ 'Wellcome Open Research',
    str_detect(doi, pattern='gatesopen') ~ 'Gates Open Research',
    str_detect(doi, pattern='openreseurope') ~ 'Open Research Europe')
  
  ## get the paper version (to match with reviewer version below)
  paper_version = as.numeric(str_sub(doi, nchar(doi), nchar(doi)))
  if(paper_version>5)cat('warning, odd version = ', paper_version, ', ', doi, '\n', sep='')
  
  ## part 1: download the XML ##
  destfile = 'paper_as_xml.xml'
  if(this_journal == 'F1000Research'){start = 'https://f1000research.com/extapi/article/xml?doi='}
  if(this_journal == 'Wellcome Open Research'){start = 'https://wellcomeopenresearch.org/extapi/article/xml?doi='}
  if(this_journal == 'Gates Open Research'){start = 'https://gatesopenresearch.org/extapi/article/xml?doi='}
  if(this_journal == 'Open Research Europe'){start = 'https://open-research-europe.ec.europa.eu/extapi/article/xml?doi='}
  url = paste(start, doi, sep='')
  e = tryCatch(download_xml(url = url, file = destfile), error=function(e){'xml not available'}) # need try catch for occasional 404 error, eg., 10.12688/f1000research.135533.1
  if(e == 'xml not available'){
    to_return = list()
    to_return$reviewers = NULL
    to_return$references = NULL
    to_return$excluded = TRUE
    to_return$excluded_reason = "XML not available"
    return(to_return)
  }
  # read the XML
  paper = read_xml(destfile, encoding='UTF-8')
  file.remove(destfile) # clean up to avoid file being read in for next paper
  
  # cross-check the paper's doi
  check_doi = xml_text(xml_find_all(paper, "//article-meta//article-id[@pub-id-type='doi']"))
  if(check_doi != doi){cat('Error, DOI does not match: ', doi, '\n', sep='')}
  
  # exclude F1000 faculty reviews as peer review process is different, e.g., 10.12688/f1000research.6398.1
  if(this_journal == 'F1000Research'){
    notes = xml_text(xml_find_all(paper, '//front//notes'))
    if(length(notes)==0){is_faculty_review = FALSE} # no need to look if there are no notes
    if(length(notes)>0){is_faculty_review = str_detect(pattern='F1000 Faculty Reviews are commissioned', notes)}
    location = xml_text(xml_find_all(paper, '//article-meta//elocation-id'))
    is_faculty_review_location = str_detect(location, pattern = 'Faculty Rev')
    if(is_faculty_review == TRUE | is_faculty_review_location == TRUE){
      to_return = list()
      to_return$reviewers = NULL
      to_return$references = NULL
      to_return$excluded = TRUE
      to_return$excluded_reason = "Faculty Review"
      return(to_return)
    }
  } # end of if
  
  ## part 2: get the DOIs and PMIDs from the paper's reference list ##
  xml_remove(xml_find_all(paper, "front")) # abstract, etc ; remove things not needed
  front <- xml_find_all(paper, "front") # front matter
  back <- xml_find_all(paper, "back") # back matter ...
  ids <- xml_find_all(back, ".//ref-list//ref") # ... references in back matter ...
  n_refs = length(ids)
  doi_data = NULL
  if(n_refs > 0){ # small number with no references, e.g., 10.12688/wellcomeopenres.20057.2
    for (k in 1:n_refs){
      # get PMID as back-up to DOI
      pmid = xml_text(xml_find_all(xml_children(ids[[k]]),'.//pub-id[@pub-id-type="pmid"]'))[1] # added [1] as safety net for publisher errors, e.g., reference 55 of https://f1000research.com/articles/11-177
      if(length(pmid) ==0){pmid=NA}
      # get DOI
      this_doi = xml_text(xml_find_all(xml_children(ids[[k]]),'.//pub-id[@pub-id-type="doi"]'))[1] # added [1] as safety net for publisher errors, e.g., reference 8 of https://wellcomeopenresearch.org/articles/6-228
      # get alternative DOI from hyper-links
      if(length(this_doi) == 0){
        this_doi = xml_attr(xml_find_all(xml_children(ids[[k]]),'.//ext-link[@ext-link-type="uri"]'),'href') # get link
        this_doi = str_remove(this_doi, 'http://dx.doi.org/') 
        this_doi = ifelse(str_detect(pattern = '^10\\.', this_doi), this_doi, NA) # excluded non-DOIs
        this_doi = this_doi[1] # safety net in case of doubles
      }
      if(length(this_doi) == 0){this_doi = NA}
      if(!is.na(this_doi) | !is.na(pmid) ){ # only add to data if there's a DOI or PMID
        frame = data.frame(doi = doi, cited_pmid = pmid, cited_doi = this_doi) %>%
          mutate(cited_doi = str_squish(cited_doi)) 
        doi_data = bind_rows(doi_data, frame) 
      }
    }
  }
  # stop if no references 
  if(is.null(doi_data) == TRUE){
    to_return = list()
    to_return$reviewers = NULL
    to_return$references = NULL
    to_return$excluded = TRUE
    to_return$excluded_reason = "No references"
    return(to_return)
  }
  # monitor the number of references, and the number with a DOI or PMID
  n_doi_pmid = nrow(doi_data)
  ref_meta_data = data.frame(doi, n_refs, n_doi_pmid)
  
  ## part 3: get the referees' names, affiliations, review text, and any papers they cite ##
  xml_remove(xml_find_all(back, "//article-meta")) # remove authors
  reports <- xml_find_all(back, "//sub-article[@article-type='ref-report']") # ... reports in back matter ...
  if(length(reports)==0){
    reports <- xml_find_all(back, "//sub-article[@article-type='reviewer-report']") # ... reports in back matter ...
  }
  n_reviewers = length(reports)
  reviewers_names = papers_reviewers_cited  = NULL
  if(n_reviewers > 0){ # small number with no reviewers, e.g., https://f1000research.com/articles/12-1568
    for (k in 1:n_reviewers){
      xml_remove(xml_find_all(reports[[k]],'//label')) # removing label number from affiliation
      xml_remove(xml_find_all(reports[[k]],"//sub-article[@article-type='response']")) # remove authors' response to reviewer 
      role = xml_text(xml_find_all(xml_children(reports[[k]]), './/role'))
      version = xml_text(xml_find_all(xml_children(reports[[k]]), './/title-group//article-title'))
      version = str_remove_all(version, pattern = 'Reviewer response for version ') # just extract version number
      version = as.numeric(version)
      # only proceed if reviewer version matches paper version
      if(version != paper_version){next}
      #
      affiliation = xml_text(xml_find_all(xml_children(reports[[k]]), './/aff'))
      affiliation = affiliation[1] # just take first affiliation
      first_name = xml_text(xml_find_all(xml_children(reports[[k]]), './/given-names'))
      surname = xml_text(xml_find_all(xml_children(reports[[k]]), './/surname'))
      # clean names and make combined name
      first_name = str_remove_all(first_name, "^Reader\\.?") # 
      first_name = str_remove_all(first_name, "^Chair\\.?") # 
      first_name = str_remove_all(first_name, "^Dean\\.?") # 
      first_name = str_remove_all(first_name, "^Senior\\.?") # 
      first_name = str_remove_all(first_name, "^Lecturer\\.?") # 
      first_name = str_remove_all(first_name, "^Assistant\\.? Prof(essor)?\\.?") # 
      first_name = str_remove_all(first_name, "^Assoc(iate)?\\.? Prof(essor)?\\.?") # 
      first_name = str_remove_all(first_name, "^(Adjunct.|Visiting.|Emeritus.|Distinguished.)?Prof(essor)?\\.?") # 
      first_name = str_remove_all(first_name, "^Dr\\.?") # 
      surname = str_remove_all(surname, ", Ph\\.?D\\.?") # !
      surname = str_remove_all(surname, ", FMWI")
      # small fix to a few names that include qualifications with a comma (remove everything after comma)
      surname = str_remove_all(surname, pattern = after_comma)
      first_name = str_remove_all(first_name, pattern = after_comma)
      # final tidy
      first_name = str_squish(first_name)
      surname = str_squish(surname)
      name = paste(first_name, surname, sep=' ') # make one variable for name
      #
      recommendation = xml_text(xml_find_all(xml_children(reports[[k]]), './/meta-value'))
      orcid = xml_text(xml_find_all(reports[[k]],'.//uri[@content-type="orcid"]'))
      date = xml_find_all(xml_children(reports[[k]]), './/pub-date')
      day = xml_text(xml_find_first(date, './/day'))
      month = xml_text(xml_find_first(date, './/month'))
      year = xml_text(xml_find_first(date, './/year'))
      date = as.Date(paste(c(year,month,day), collapse='-'))
      if(length(orcid)==0){orcid = NA}
      ## get papers cited by reviewers and add to data
      cited_papers = xml_find_all(xml_children(reports[[k]]), './/ref-list//ref')
      n_papers_cited_by_reviewer = length(cited_papers)
      n_papers_cited_doi_pmid = 0
      if(n_papers_cited_by_reviewer > 0){
        for (i in 1:n_papers_cited_by_reviewer){
          this_paper = cited_papers[[i]]
          cited_doi =  xml_text(xml_find_all(this_paper, './/pub-id[@pub-id-type="doi"]'))
          if(length(cited_doi) == 0){cited_doi = NA}
          cited_pmid = xml_text(xml_find_all(this_paper, './/pub-id[@pub-id-type="pmid"]'))
          if(length(cited_pmid) == 0){cited_pmid = NA}
          if(!is.na(cited_doi) | !is.na(cited_pmid)){ # as long as one is not missing
            n_papers_cited_doi_pmid = n_papers_cited_doi_pmid + 1
            dframe = data.frame(review_number = k, name = name, first_name = first_name, surname = surname, 
                                version = version, doi = doi, cited_doi = cited_doi, cited_pmid = cited_pmid)
            papers_reviewers_cited = bind_rows(papers_reviewers_cited, dframe)
          }
        }
      }
      # very occasional fix for co-referees as some do not have ORCID
      if(length(orcid) != length(surname)){
        ###
        all_names = xml_find_all(xml_children(reports[[k]]),'.//contrib-group//contrib')
        n_names = length(all_names)
        orcid = rep(NA, n_names)
        for (i in 1:n_names){
          this_orcid = xml_text(xml_find_all(all_names[[i]],'.//uri[@content-type="orcid"]'))
          if(length(this_orcid) ==0){this_orcid=NA}
          orcid[i] = this_orcid
        }
      }
      
      ## get review text wihtout questions - questions are too varied to be useful
      review_in_parts = xml_find_first(reports[[k]], './/body')
      rlist = as_list(xml_children(review_in_parts)) # break into children
      list_size = length(rlist) - 1 # last item is always conflict
      # do not include questions or expertise
      rtext = NULL
      for (z in 1:list_size){
        n_elements = length(rlist[[z]])
        # paste elements together without formatting (does not remove all formatting)
        this_text = NULL
        for (j in 1:n_elements){
          this_text = paste(this_text, paste(unlist(rlist[[z]][j]), collapse=' '))
        }
        #
        if(this_text == ' '){next} # skip if empty
        # look ahead for question
        is_question_ahead = is_question_behind = is_expert_ahead = is_expert_behind = FALSE
        this_text_next = paste(rlist[[z+1]], collapse= ' ')
        is_question_ahead = str_detect(this_text, '\\?') & str_detect(this_text_next, ('^ ?(Yes|No|Partly|Not applicable|I cannot comment)'))
        is_expert_ahead = str_detect(this_text, '^ ?Reviewer Expertise:')
        # look behind for answer
        if(z > 1){
          this_text_previous = paste(rlist[[z-1]], collapse= ' ')
          is_question_behind= str_detect(this_text_previous, '\\?') & str_detect(this_text, ('^ ?(Yes|No|Partly|Not applicable|I cannot comment)'))
          is_expert_behind = str_detect(this_text_previous, '^ ?Reviewer Expertise:')
        }
        if(is_question_ahead == TRUE | is_question_behind == TRUE | is_expert_ahead == TRUE | is_expert_behind == TRUE ){next}
        # 
        rtext = paste(rtext, this_text)
      }
      if(is.null(rtext)){rtext=''} # for empty reviews, e.g., 10.12688/f1000research.18802.2
      # tidy up text
      rtext = str_replace_all(rtext, 'list\\(?\\"|\\"\\)', '.') # remove lists from above
      rtext = str_replace_all(rtext, pattern='\\.(?!(?<=\\d\\.)\\d) ?', '. ') # add spaces after punctuation
      rtext = str_replace_all(rtext, pattern='\\?', '? ') # add spaces after punctuation
      rtext = str_replace_all(rtext, pattern='\\!', '! ') # add spaces after punctuation
      rtext = str_replace_all(rtext, pattern='\\. ? ? ?\\.', '.') # double full-stops
      rtext = str_replace_all(rtext, pattern='e\\. g\\.? ', 'e.g.')
      rtext = str_replace_all(rtext, pattern='i\\. e\\.? ', 'i.e.')
      rtext = str_squish(rtext)
      
      # make frame; need review number due to co-referees
      frame = data.frame(review_number = k, date, role, version, name, first_name, surname, 
                         affiliation, recommendation, orcid, n_papers_cited_by_reviewer, n_papers_cited_doi_pmid, rtext)
      # don't need repeated text for co-referee
      index = which(frame$role != 'Referee')
      if(length(index)>0){
        frame$rtext[index] = ''
      }
      
      # concatenate
      reviewers_names = bind_rows(reviewers_names, frame)
      
    }
    if(!is.null(reviewers_names)){
      reviewers_names = unique(reviewers_names) %>%
        mutate(doi = doi)
    }
  } # end of if
  
  # return
  to_return = list()
  to_return$reviewers = reviewers_names
  to_return$ref_meta_data = ref_meta_data
  to_return$papers_references_dois = doi_data # reference lists
  to_return$papers_reviewers_cited = papers_reviewers_cited
  return(to_return)
} # end of function

