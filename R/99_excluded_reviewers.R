# 99_excluded_reviewers.R
# analysis of what reviewers were excluded, looking for bias in missing data
# May 2025
library(dplyr)
library(glmnet) # for lasso
TeachingDemos::char2seed('betis')

# get the data
load('data/4_reviewers_papers.RData')

# create index

# dependent variables
depvars = c('date','role','version','orcid_missing','recommendation','country_code')

formula = paste('excluded ~', paste(devpars, collapse = ' + ', sep=''), sep='')
x = model.matrix(formula, data = excluded)


#
glmnet = glmnet(x = x,
               y = y,
               family = 'binomial')

# cross-validation
