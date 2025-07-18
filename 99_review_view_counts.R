# 99_review_view_counts.R
# how many times are the reviews viewed?
# June 2025
library(nimble)
library(readxl)
library(dplyr)
library(ggplot2)

# date that views were downloaded
today = as.Date('2025-06-08')

# get the data on views
not_cited = read_excel('checks/6_random_checks_complete.xlsx', sheet = 1, col_names =TRUE) %>%
  unique() # safety net for duplicates
cited = read_excel('checks/6_random_checks_complete.xlsx', sheet = 2, col_names =TRUE) %>%
  unique() # safety net for duplicates
#
views = bind_rows(not_cited, cited) %>%
  select(doi, name, views)

# get the date review was written
load('data/3_reviewers_names.RData')
for_model = left_join(views, reviewers_names, by=c('doi','name')) %>%
  mutate(time = as.numeric(today - date)/365.25) %>%
  filter(!is.na(views))

# model rate of reviews - to do
code <- nimbleCode({
  ## Likelihood
  for (i in 1:N){ # loop through reviews
    views[i] ~ dpois(mu[i])
    log(mu[i]) <- log(time[i]) + alpha
  }
  alpha ~ dnorm(0, 0.001)
})

## data
constants <- list(N = nrow(for_model),
                  time = for_model$time)
data <- list(views = for_model$views)

## initial values
inits <- list(alpha = 0)

# parameters to store
parms = c('alpha')

# models
model <- nimbleModel(code, 
                     data = data, 
                     inits = inits, 
                     constants = constants)

# MCMC samples
MCMC = 4000; n.chains = 2; thin = 3
seeds = rep(0,2)
seeds[1] = TeachingDemos::char2seed('andorra')
seeds[2] = TeachingDemos::char2seed('senegal')
mcmc_out <- nimbleMCMC(model = model,
                       inits = inits,
                       monitors = parms,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seeds,
                       WAIC = FALSE)
mcmc_out$summary$all.chains # for alpha, which is the mean rate per year
plot(mcmc_out$samples$chain1[,1])
points(mcmc_out$samples$chain2[,1], col='red')

# views could also be a function of the number of authors and the journal; not modelled

## histogram 
# make bins manually
bin_size = 10
for_histo = mutate(for_model, 
                   binned = floor(views/bin_size)) %>% # [0,10), [10,20), etc
  select(doi, views, binned)
counts = group_by(for_histo, binned) %>%
  tally() %>%
  ungroup() 

# plot
x.breaks = seq(1,17)
x.labels = (x.breaks-1)*10
hplot = ggplot(data = counts, aes(x = binned, y = n))+
  geom_bar(stat='identity', col='grey22', fill='skyblue', width=0.99, size=0.1)+ # size for thinner lines around bars
  scale_x_continuous(breaks = x.breaks - 1.5, # nudge breaks to start of bar
                     labels = x.labels)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  ylab('Count')+
  xlab('Number of views')
hplot

# export
ggsave('figures/99_review_counts.jpg', plot = hplot, width = 4.5, height = 4, units='in', dpi = 500)
