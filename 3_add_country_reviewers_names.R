# 3_add_country_reviewers_names.R
# add reviewers' country and country code from affiliation
# March 2025
library(dplyr)
library(countrycode)
library(janitor)
library(stringr)
source('R/add_country_reviewers_names.R')
source('R/add_country.R')

# get the data from 2_get_xml_details_and_combine.R
load('data/2_xml_data.RData')
remove(ref_meta_data, papers_reviewers_cited, reference_dois, excluded) # not needed

# main function
reviewers_names = add_country_reviewers_names(reviewers_names)

# save
save(reviewers_names, file = 'data/3_reviewers_names.RData')
