# 99_check_odds.R
# check the odds ratios using a contingency table; confirms conditional logistic regression results
# June 2025

library(dplyr)

# data from 5_match_papers_reviewers
load('data/5_analysis_data.RData')

# make the table for version 1
tablev1 = filter(matched, version==1) %>% # 
  mutate(any_self_cite = self_cited_count>0,
         outcome = recommendation == 'approve') %>%
  group_by(any_self_cite, outcome) %>%
  tally() %>%
  group_by(any_self_cite) %>%
  mutate(p = prop.table(n))
tablev1

# make the table for version 2
tablev2 = filter(matched, version!=1) %>% # looking at big difference in second version
  mutate(any_self_cite = self_cited_count>0,
         outcome = recommendation == 'approve') %>%
  group_by(any_self_cite, outcome) %>%
  tally() %>%
  group_by(any_self_cite) %>%
  mutate(p = prop.table(n))
tablev2
