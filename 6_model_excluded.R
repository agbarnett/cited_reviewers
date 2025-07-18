# 6_model_excluded.R
# model reviewers who were excluded because of no openalex record, looking for bias
# June 2025
library(glmnet) 
library(dplyr)
library(stringr)

# data from 4_reviewers_papers_openalex.R
load('data/4_reviewers_papers.RData')
remove(reviewers_papers) # not needed
# scale date to years
excluded = mutate(excluded, 
                  role = str_remove(role, '[1-9]'), # combine all co-referees
                  date = (as.numeric(date) - 19262 )/365.25  )

# make formula, no intercept
independent_vars = c('version','role','country','date')
formula = paste("reason ~ - 1 +", paste(independent_vars, collapse = ' + ', sep=''), sep='')
# could add recommendation

# get X matrix
x = model.matrix(as.formula(formula), data = excluded)
x = x[,colnames(x)!="roleCo-referee"] # avoid fitting reference category
y = as.numeric(excluded$reason == 'Not excluded')

#
TeachingDemos::char2seed('inter')
# model
fit <- glmnet(x, y, family = binomial)
plot(fit)
# cross-validation
cvfit <- cv.glmnet(x, y, family = binomial)
plot(cvfit)
coef(cvfit, s = "lambda.1se")

# export to table for latex

