# 99_examine_selected_reviews.R
# look at interaction between version 1 and 2
# June 2025
library(dplyr)
library(tidyr)
library(contrast)

# data from 5_match_papers_reviewers
load('data/5_analysis_data.RData')

# version 1
one = filter(matched, version == 1) %>%
  select(doi, name, self_cited_count, matches, recommendation) %>%
  mutate(doi = str_remove(doi, '\\.v?([1-9]|10)$')) %>% # remove version from DOI
  rename('recommendation_1' = 'recommendation',
         'matches_1' = 'matches',
         'self_cited_count_1' = 'self_cited_count')
# version 2
two = filter(matched, version == 2) %>%
  mutate(doi = str_remove(doi, '\\.v?([1-9]|10)$')) %>% # remove version from DOI
  select(doi, name, self_cited_count, matches, recommendation) %>% # remove version from DOI
  rename('recommendation_2' = 'recommendation')
#
both = full_join(one, two, by=c('doi','name'))
table(both$recommendation_1, both$recommendation_2, useNA = 'always')

# not cited in version 1, asked for self-citation
scenario1 = filter(both, matches_1 == 0, self_cited_count_1 > 0, !is.na(matches))
tab1 = mutate(scenario1, matches = matches > 0) %>% # cited in second version
  group_by(matches, recommendation_2) %>%
  tally() %>%
  group_by(matches) %>%
  mutate(p = prop.table(n))
tab1

# get difference using 2x2 table
tab2 = mutate(scenario1, matches = matches > 0, # cited in second version
              recommendation_2 = ifelse(recommendation_2 == 'approve-with-reservations', 'reject', recommendation_2)) %>% # combine reservations and reject
  group_by(matches, recommendation_2) %>%
  tally() %>%
  group_by(matches) %>%
  mutate(p = prop.table(n))
tab2

# run model to get odds ratio
model = glm(I(matches>0) ~ I(recommendation_2=='approve'), family=binomial(link='logit'), data = scenario1)
# predicted difference
diff <- 
  contrast(model, 
           list(recommendation_2 = "approve"),
           list(recommendation_2 = "reject")
  )
print(diff, X = TRUE) # log odds
exp(c(1.253446, 0.6944989, 1.812394))
