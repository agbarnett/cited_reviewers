# 6_models_not_cited.R
# examine associations between non-self citations and peer review decisions
# all models split by version as authors know reviewers after first version
# outcomes are accept vs reservations/reject, accept/reservations vs reject
# August 2025
library(survival) # for conditional logistic regression
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(showtext) # for text size issues (plus next two lines)
showtext_auto()
showtext_opts(dpi = 500)
library(xtable) # for latex
source('R/clogit_model.R') # runs the conditional logistic regression models
source('R/plot_diff.R') # plots estimates
source('R/g_legend.R') # for separate legend in grid.arrange plots

# odds ratio label used in plots
ylab = "Odds ratio (Approved \u2192 Reservations \u2192 Not approved)" # using symbol for the right arrow

# data from 5_match_papers_reviewers.R
load('data/5_analysis_data.RData')

# make the non self cited count
matched = mutate(matched,
                 non_self_cited_count = n_reviewer_cited - self_cited_count, # reviewer citation counts after removing self-citations
                 non_self_cited_count = ifelse(non_self_cited_count<0, 0, non_self_cited_count)) # fix one error

## run with and without co-reviewers
# run reviewer asking for citations, but not self-citations (results3)
results3 = run_clogit(indata = matched, predictor='non_self_cited_count', countries = NULL)
# reviewers only (excluding co-reviewers)
results3_only_referees = run_clogit(indata = matched, predictor='non_self_cited_count', co_reviews = FALSE, countries = NULL)

# redo main figures with probabilities
source('6_small_figure_rearranged3.R')
