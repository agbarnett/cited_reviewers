# 4_reviewers_papers_openalex.R
# get reviewers' papers from Open Alex
# put individual results in open_alex_results and open_alex_exclusions folders
# then concatenate data
# March 2025
library(openalexR)
library(stringr)
library(dplyr)
# key functions:
source('R/fetch_open_alex_reviewer_data.R')
source('R/find_author.R')
source('R/find_institution.R')
source('R/match_words.R')
source('R/concatenate_reviewers_papers.R')

## part 1, get papers from OpenAlex

# get reviewers names from 3_add_country_reviewers_names.R
load('data/3_reviewers_names.RData')
reviewers_names = unique(reviewers_names) # small number of duplicates

# add co-referee #2, #3, etc to a small number with multiple co-referees
reviewers_names = group_by(reviewers_names, doi, review_number, role) %>%
  mutate(slice = 1:n()) %>%
  ungroup() %>%
  mutate(role = ifelse(slice > 1, paste(role, slice, sep=''), role)) %>%
  select(-'slice')

# find existing files (clear directory if doing a major re-run); better to use exclusions as that has both included and excluded reviewers
open_alex_search_results = dir('open_alex_exclusions', pattern='rds$')

# big loop to get data from open alex
fetch_open_alex_reviewer_data_loop(reviewers_names, open_alex_search_results)

## part 2: put rds files together, takes a while
open_alex_search_results = dir('open_alex_results', pattern='rds$') # refresh search
open_alex_search_results = paste('open_alex_results/', open_alex_search_results, sep='')
data_list = concatenate_reviewers_papers(reviewers_names, open_alex_search_results)
# (warning is about excluded reviews)
## concatenated exclusions from rds files
open_alex_exclusion_results = dir('open_alex_exclusions', pattern='rds$') # refresh search
open_alex_exclusion_results = paste('open_alex_exclusions/', open_alex_exclusion_results, sep='')
exclusions = concatenate_reviewers_exclusions(reviewers_names, open_alex_exclusion_results)

## save
# fix my replace in doi (slash for dot)
reviewers_papers = data_list$reviewers_papers
reviewers_papers = mutate(reviewers_papers, 
                          doi = paste(str_sub(doi, 1, 8), '/', str_sub(doi, 10, nchar(doi)), sep=''))
excluded = exclusions$excluded
excluded = mutate(excluded, 
                  doi = paste(str_sub(doi, 1, 8), '/', str_sub(doi, 10, nchar(doi)), sep=''))
save(excluded, reviewers_papers, file = 'data/4_reviewers_papers.RData')
