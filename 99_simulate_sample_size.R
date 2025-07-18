# 99_simulate_sample_size.R
# simulate data to test the model, version with ordinal data
# May 2024
library(survival) # for clogit
library(broom)
library(janitor)
library(ggplot2)
library(dplyr)
source('R/run_simulation.R')

# basics
nsim = 1000
odds_ratio = 1.5 # odds ratio to detect
alpha = log(odds_ratio) # on logit scale (log odds ratio due to exposure)
true = prob(alpha) - prob(0) # difference in probability scale

# without unmeasured confounding
sim_results = NULL
for (i in 1:nsim){
  res = run_simulation_ordinal(alpha = alpha, 
                               n_papers = 2500, # half of 5000 for version 1
                               u1 = FALSE)$clogit # ordinal model
  res = mutate(res, sim = i)
  sim_results = bind_rows(sim_results, res)
}

# with unmeasured confounding
for (i in 1:nsim){
  res = run_simulation_ordinal(alpha = alpha, 
                               n_papers = 5000,
                               u1 = TRUE)$clogit # ordinal model
  res = mutate(res, sim = i)
  sim_results = bind_rows(sim_results, res)
}

## plot log odds ratios
cplot = ggplot(data = sim_results, aes(x=u1, y = estimate, fill=u1))+
  geom_hline(yintercept = alpha, lty=2, col='darkorange1')+
  geom_boxplot()+
  theme_bw()+
  ggtitle('Conditional logistic regression')+
  xlab('Confounding via quality')+
  ylab('Log odds ratio')+
  facet_wrap(~model)
cplot

## power
group_by(sim_results, sim) %>%
  mutate(over = conf.low > 0) %>% # one-sided?
  summarise(either = max(over)) %>% # either OR is positive so accept vs revise/reject or accept/revise vs reject
  ungroup() %>%
  summarise(n = n(), r = sum(either), power = 100*r/n)
