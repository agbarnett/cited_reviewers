# 5_match_papers_reviewers.R
# makes the main analysis data set
# match the papers in the papers' reference lists and reviewers' papers to find: 1) if reviewer was cited, 2) reviewer asking for citations to their papers
# June 2025
library(dplyr)
library(openalexR)
library(stringr)
library(janitor) # for duplicates
# key functions:
source('R/match_papers_reviewers.R')
source('R/open_alex_id.R')

## get the data
# from 2_get_xml_details_and_combine.R
load('data/2_xml_data.RData')
remove(excluded, reviewers_names) # not needed

# get reviewers names from 3_add_country_reviewers_names.R
load('data/3_reviewers_names.RData')
reviewers_names = unique(reviewers_names) # small number of duplicates

# reviewers_names from 4_reviewers_papers_openalex.R
load('data/4_reviewers_papers.RData')

# fix some errors in DOIs
source('5_fix_odd_errors.R')

# remove excluded reviews from reviewers names
excluded_small = filter(excluded, reason != 'Not excluded') %>% select(doi, review_number, name)
reviewers_names = anti_join(reviewers_names, excluded_small, by=c('doi','review_number','name'))

# final row number:
N = nrow(reviewers_names)
# run main function, done in batches because of potential breaks; warnings about truncated author lists
matched1 = match_papers_reviewers(
  reviewers_names = reviewers_names,
  reviewers_papers = reviewers_papers, # big data set of all the reviewers' papers
  papers_references_dois = reference_dois, # references cited in included papers
  papers_reviewers_cited = papers_reviewers_cited,
  start = 1,
  stop = 2000)
matched2 = match_papers_reviewers(
  reviewers_names = reviewers_names,
  reviewers_papers = reviewers_papers, # big data set of all the reviewers' papers
  papers_references_dois = reference_dois, # references cited in included papers
  papers_reviewers_cited = papers_reviewers_cited,
  start = 2001,
  stop = 4000)
matched3 = match_papers_reviewers(
  reviewers_names = reviewers_names,
  reviewers_papers = reviewers_papers, # big data set of all the reviewers' papers
  papers_references_dois = reference_dois, # references cited in included papers
  papers_reviewers_cited = papers_reviewers_cited,
  start = 4001,
  stop = 9000)
matched4 = match_papers_reviewers(
  reviewers_names = reviewers_names,
  reviewers_papers = reviewers_papers, # big data set of all the reviewers' papers
  papers_references_dois = reference_dois, # references cited in included papers
  papers_reviewers_cited = papers_reviewers_cited,
  start = 9001,
  stop = 14000)
matched5 = match_papers_reviewers(
  reviewers_names = reviewers_names,
  reviewers_papers = reviewers_papers, # big data set of all the reviewers' papers
  papers_references_dois = reference_dois, # references cited in included papers
  papers_reviewers_cited = papers_reviewers_cited,
  start = 14001,
  stop = 19000)
matched6 = match_papers_reviewers(
  reviewers_names = reviewers_names,
  reviewers_papers = reviewers_papers, # big data set of all the reviewers' papers
  papers_references_dois = reference_dois, # references cited in included papers
  papers_reviewers_cited = papers_reviewers_cited,
  start = 19001,
  stop = 24000)
matched7 = match_papers_reviewers(
  reviewers_names = reviewers_names,
  reviewers_papers = reviewers_papers, # big data set of all the reviewers' papers
  papers_references_dois = reference_dois, # references cited in included papers
  papers_reviewers_cited = papers_reviewers_cited,
  start = 24001,
  stop = 29000)
matched8 = match_papers_reviewers(
  reviewers_names = reviewers_names,
  reviewers_papers = reviewers_papers, # big data set of all the reviewers' papers
  papers_references_dois = reference_dois, # references cited in included papers
  papers_reviewers_cited = papers_reviewers_cited,
  start = 29001,
  stop = 32000)
matched9 = match_papers_reviewers(
  reviewers_names = reviewers_names,
  reviewers_papers = reviewers_papers, # big data set of all the reviewers' papers
  papers_references_dois = reference_dois, # references cited in included papers
  papers_reviewers_cited = papers_reviewers_cited,
  start = 32001,
  stop = N)

# show duplicate rows
matched = bind_rows(matched1, matched2, matched3, matched4, matched5, matched6, 
                    matched7, matched8, matched9)
check = get_dupes(matched) # function from janitor

# concatenate
matched = unique(matched) # remove small number of duplicates
matched = mutate(matched, name = str_remove_all(name, ',$')) # fix one name

# create article version that is relative to reviewer
matched = mutate(matched, doi_no_version = str_remove(doi, '\\.v?([1-9]|10)$'))
first_for_reviewer = group_by(matched, doi_no_version, name) %>%
  summarise(rversion = min(version)) %>%
  ungroup()
matched = left_join(matched, first_for_reviewer, by=c('doi_no_version', 'name')) %>%
  mutate(rversion = version - rversion + 1) %>%
  select(-'doi_no_version')

# save
save(matched, file = 'data/5_analysis_data.RData')

