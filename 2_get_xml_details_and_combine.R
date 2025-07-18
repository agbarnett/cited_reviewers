# 2_get_xml_details_and_combine.R
# Get detailed XML data for papers from the journals' APIs
# Then combine all available files
# May 2025
library(dplyr)
library(stringr)
library(xml2)
# key functions: 
source('R/extract_xml_data.R')
source('R/extract_details.R')
source('R/concatenate_xml_data.R')

# avoid repeating search for existing results (clear when doing a re-run)
xml_search_results = dir('xml_results')

# get DOIs
load('data/0_api_search.RData') # from 0_find_papers_and_get_dois.R

# big loop, takes a while
loop_extract_xml_data(all_dois, xml_search_results)

# update search
xml_search_results = dir('xml_results')
if(length(xml_search_results) != nrow(all_dois)){cat('Error, missing papers.\n')}

# now concatenate results
xml_data = concatenate_xml_data(xml_search_results)

# save 
attach(xml_data) # get elements of the list
# final tidy
reference_dois = mutate(reference_dois,
                        cited_pmid = str_squish(cited_pmid),
                        cited_doi = str_squish(cited_doi))
save(ref_meta_data, 
     reviewers_names, 
     papers_reviewers_cited,
     reference_dois, 
     excluded, file = 'data/2_xml_data.RData')

# export all DOIs for Gerald (June 2025)
#source('2_export_gerald.R')

