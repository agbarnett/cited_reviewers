# 6_models.R
# main models examining associations between citations and peer review decisions
# all models split by version as authors know reviewers after first version
# cannot include journal as we match on DOI
# outcomes are accept vs reservations/reject, accept/reservations vs reject
# June 2025
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

# get the ten most common countries for leave-one-out analysis
top_ten_countries = group_by(matched, country) %>%
  tally() %>%
  arrange(-n) %>%
  slice(1:10) %>%
  pull(country)

## run with and without co-reviewers
# run reviewer being cited (results1) and reviewer asking for citations (results2)
results1 = run_clogit(indata = matched, predictor='matches', countries = top_ten_countries)
results2 = run_clogit(indata = matched, predictor='self_cited_count', countries = top_ten_countries)
# reviewers only (excluding co-reviewers)
results1_only_referees = run_clogit(indata = matched, predictor='matches', co_reviews = FALSE, countries = top_ten_countries)
results2_only_referees = run_clogit(indata = matched, predictor='self_cited_count', co_reviews = FALSE, countries = top_ten_countries)

# redo main figures with probabilities
source('6_small_figure_rearranged1.R')
source('6_small_figure_rearranged2.R')
# single figure for linear results
source('6_linear_plot.R')


# plot to shows the difference in estimates for referees and co-referees
dplot1 = plot_diff(co_reviewers = results1$results, referees_only = results1_only_referees$results)
dplot2 = plot_diff(co_reviewers = results2$results, referees_only = results2_only_referees$results)
ggsave(filename = 'figures/6_reviewer_cited_differences.jpg', dplot1,
       width = 6, height = 6, units = 'in', dpi = 500)
ggsave(filename = 'figures/6_self_cited_differences.jpg', dplot2,
       width = 6, height = 6, units = 'in', dpi = 500)

## tabulate results in latex (both referees and co-referees)
# results for log-transformed reviewer counts, as this was best
for_table1 = filter(results1$results, clabel == 'log')
for_table2 = filter(results2$results, clabel == 'log')
for_table3 = filter(results1_only_referees$results, clabel == 'log')
for_table4 = filter(results2_only_referees$results, clabel == 'log')
to_table = bind_rows(for_table1, for_table2, for_table3, for_table4, .id = 'model') %>%
  mutate(question = ifelse(model %in% c(1,3), 'Cited', 'Self-cited'),
         referees = ifelse(model %in% c(3,4), 'Referee only', 'Both'),
         type = ifelse(type == 'linear', "Linear", "Any vs none"),
         version = factor(version, levels=1:2, labels=c('Version = 1','Versions = 2+')),
         outcome = factor(outcome, levels=1:2, labels=c(l1,l2)),
         coef = round(coef*100)/100,
         lower = round(lower*100)/100,
         upper = round(upper*100)/100,
         cell = paste(coef, ' (', lower, ', ', upper, ')', sep='')) %>%
  select(question, type, version, outcome, referees, cell) 
print(xtable(to_table, digits=2), include.rownames=FALSE, hline.after=FALSE, file = "results/6_models.tex")

## examine AIC, is there confounding, which outcome is better
# a) cited
for_aic = mutate(results1_only_referees$results,
                 type = ifelse(type == 'linear', "Linear citations (0,1,2,...)", "Any citations vs none"),
                 version = factor(version, levels=1:2, labels=c('Version = 1','Versions = 2+')),
                 outcome = factor(outcome, levels=1:2, labels=c(l1, l2))) %>%
  filter(clabel != 'Country') # is so far from other models, clearly terrible
bplot = ggplot(data = for_aic, aes(x = type, y= aic, label = clabel))+
  geom_point(col = 'darkred')+
  geom_text(adj = 1.5, col = 'darkred')+
  facet_wrap(~version+outcome, scales='free')+
  theme_bw()+
  theme(strip.text = element_text(margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "lines")), # reduce spacing in facet labels
        panel.spacing = unit(0.5, "lines"))+ # reduce size of spacing between facets
  ylab('AIC')+
  xlab(NULL)
bplot
# results in table too
to_table = select(for_aic, type, clabel, version, outcome, aic)
print(xtable(to_table, digits=4), include.rownames=FALSE, hline.after=FALSE, file = "results/6_aic_cited.tex")
# export figure
ggsave(filename = 'figures/6_aic_works_count_cited.jpg', bplot,
       width = 6, height = 6, units = 'in', dpi = 500)

# b) self-cited
for_aic = mutate(results2_only_referees$results,
                 type = ifelse(type == 'linear', "Linear", "Any vs none"),
                 version = factor(version, levels=1:2, labels=c('Version = 1','Versions = 2+')),
                 outcome = factor(outcome, levels=1:2, labels=c(l1, l2))) %>%
  filter(clabel != 'Country')
bplot = ggplot(data = for_aic, aes(x = type, y= aic, label = clabel))+
  geom_point(col = 'navy')+
  geom_text(adj = 1.5, col = 'navy')+
  facet_wrap(~version+outcome, scales='free')+
  theme_bw()+
  theme(strip.text = element_text(margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "lines")), # reduce spacing in facet labels
        panel.spacing = unit(0.5, "lines"))+ # reduce size of spacing between facets
  ylab('AIC')+
  xlab(NULL)
bplot
# export
ggsave(filename = 'figures/6_aic_works_count_self_cited.jpg', bplot,
       width = 6, height = 6, units = 'in', dpi = 500)

## plot effect estimates for different versions of the potential confounder - shows little difference
# a) being cited
for_plot = filter(results1_only_referees$results, !confounder %in% top_ten_countries) %>%
           mutate(Outcome = ifelse(outcome == 1, l1, l2),
                  Version = factor(version, levels=1:2, labels=c('Version = 1','Versions = 2+')))
eplot = ggplot(data = for_plot, aes(x = clabel, y = coef, ymin = lower, ymax = upper)) +
  geom_point(col = 'darkred')+
  geom_errorbar(width = 0, col = 'darkred')+
  scale_y_log10()+
  scale_x_discrete(labels = rev(c('None','-2','-1','log','1','2','3')))+ # present transforms in order
  theme_bw() +
  theme(strip.text = element_text(size = 8, 
                                  margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "lines")), # reduce spacing in facet labels
        panel.spacing = unit(0.5, "lines"))+ # reduce size of spacing between facets
  xlab('Fractional polynomial transformation')+
  ylab(ylab)+
  facet_wrap(~Version+Outcome+facet, scales='free')+
  coord_flip()
eplot
# export
ggsave(filename = 'figures/6_estimates_adjusted_cited.jpg', eplot,
       width = 6, height = 6, units = 'in', dpi = 500)
#
# b) self-citations
for_plot = filter(results2_only_referees$results, !confounder %in% top_ten_countries) %>%
  mutate(Outcome = ifelse(outcome == 1, l1, l2),
         Version = factor(version, levels=1:2, labels=c('Version = 1','Versions = 2+')))
eplot = ggplot(data = for_plot, aes(x = clabel, y = coef, ymin = lower, ymax = upper)) +
  geom_point(col = 'navy')+
  geom_errorbar(width = 0, col = 'navy')+
  scale_y_log10()+
  scale_x_discrete(labels = rev(c('None','-2','-1','log','1','2','3')))+ # present transforms in order
  theme_bw() +
  theme(strip.text = element_text(size = 8, 
                                  margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "lines")), # reduce spacing in facet labels
        panel.spacing = unit(0.5, "lines"))+ # reduce size of spacing between facets
  xlab('Fractional polynomial transformation')+
  ylab(ylab)+
  facet_wrap(~Version+Outcome+facet, scales='free')+
  coord_flip()
eplot
# export
ggsave(filename = 'figures/6_estimates_adjusted_self_cited.jpg', eplot,
       width = 6, height = 6, units = 'in', dpi = 500)


#### results for leave-one-out country analysis ####
# reviewer cited
countries_to_plot = filter(results1_only_referees$results, confounder %in% c('', top_ten_countries)) %>%
  mutate(confounder = ifelse(confounder=='', 'All countries', confounder),
         Outcome = ifelse(outcome == 1, l1, l2),
         Version = paste('Version = ', version, sep=''))
colour = "goldenrod3"
cplot = ggplot(data = countries_to_plot, aes(x = confounder, y = coef, ymin = lower, ymax = upper)) +
  geom_point(col = colour)+
  geom_errorbar(width = 0, col = colour)+
  scale_y_log10()+
  theme_bw() +
  theme(strip.text = element_text(size = 8, 
                                  margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "lines")), # reduce spacing in facet labels
        panel.spacing = unit(0.5, "lines"))+ # reduce size of spacing between facets
  xlab(NULL)+
  ylab(ylab)+
  facet_wrap(~Version+Outcome+facet, scales='free')+
  coord_flip()
cplot
# export
ggsave(filename = 'figures/6_leave_out_country_cited.jpg', cplot,
       width = 7.4, height = 7.2, units = 'in', dpi = 500)
# self-cited
countries_to_plot = filter(results2_only_referees$results, confounder %in% c('', top_ten_countries)) %>%
  mutate(confounder = ifelse(confounder=='', 'All countries', confounder),
         Outcome = ifelse(outcome == 1, l1, l2),
         Version = ifelse(version == 1, "Version = 1", 'Versions = 2+'))
colour = "goldenrod3"
cplot = ggplot(data = countries_to_plot, aes(x = confounder, y = coef, ymin = lower, ymax = upper)) +
  geom_point(col = colour)+
  geom_errorbar(width = 0, col = colour)+
  scale_y_log10()+
  theme_bw() +
  theme(strip.text = element_text(size = 8, 
                                  margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "lines")), # reduce spacing in facet labels
        panel.spacing = unit(0.5, "lines"))+ # reduce size of spacing between facets
  xlab(NULL)+
  ylab(ylab)+
  facet_wrap(~Version+Outcome+facet, scales='free')+
  coord_flip()
cplot
# export
ggsave(filename = 'figures/6_leave_out_country_self_cited.jpg', cplot,
       width = 7.4, height = 7.2, units = 'in', dpi = 500)


## compare fit of two model types (linear vs binary) ##
library(tidyr)
fit_compare1 = filter(results1_only_referees$results, clabel=='log') %>%
  select(co_reviews, version, outcome, type, aic) %>%
  pivot_wider(values_from = 'aic', names_from = 'type') 
fit_compare1a = filter(results1$results, clabel=='log') %>%
  select(co_reviews, version, outcome, type, aic) %>%
  pivot_wider(values_from = 'aic', names_from = 'type')
fit_compare2 = filter(results2_only_referees$results, clabel=='log') %>%
  select(co_reviews, version, outcome, type, aic) %>%
  pivot_wider(values_from = 'aic', names_from = 'type') 
fit_compare2a = filter(results2$results, clabel=='log') %>%
  select(co_reviews, version, outcome, type, aic) %>%
  pivot_wider(values_from = 'aic', names_from = 'type') 
to_table = bind_rows(fit_compare1, fit_compare1a, fit_compare2, fit_compare2a) %>%
  mutate(diff = linear - any_vs_none,
         version = ifelse(version==1,'1','2+'),
         outcome = ifelse(outcome==1,'A vs R/R','A/R vs R'),
         co_reviews = ifelse(co_reviews==TRUE,'Yes','No')) %>%
  rename('Difference' = 'diff',
         'Co-reviewers included' = 'co_reviews')
# export to latex
print(xtable(to_table, digits=1), include.rownames=FALSE, hline.after=FALSE, file = "results/6_aic_models.tex")
