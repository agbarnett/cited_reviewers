# 6_model_self_by_country.R
# Bayesian model of self-citations by country, called by 6_plots.Rmd
# June 2025
library(nimble)
library(stringr)

# prepare data
for_model = select(matched, country, self_cited_count) %>%
  mutate(countrynum = as.numeric(as.factor(country)))

# MCMC set-up
seeds = rep(0,2)
seeds[1] = TeachingDemos::char2seed('spain')
seeds[2] = TeachingDemos::char2seed('netherlands')
MCMC = 5000
thin = 10 # slow mixing
n.chains = 2

## define the model
code <- nimbleCode({
  for (i in 1:N) {
    number[i] ~ dpois(mu[i])
    log(mu[i]) <- alpha + beta[country[i]]
  }
  # random intercept for each country - no need to centre
  for(j in 1:M){
    beta[j] ~ dnorm(0, tau)
  }
  tau ~ dgamma(0.1, 0.1)
  alpha ~ dnorm(0, var = 10000)
})

## data
M = max(for_model$countrynum)
constants <- list(N = nrow(for_model),
                  country = for_model$countrynum,
                  M = M)
data <- list(number = for_model$self_cited_count) 

## initial values
inits <- list(tau = 1,
              alpha = log(0.2), # ball park start
              beta = rep(0, M))

# parameters to store
parms = c('beta','tau','alpha')

# models
model <- nimbleModel(code, 
                     data = data, 
                     inits = inits, 
                     constants = constants)

# MCMC samples
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

# extract summary
table = as.data.frame(mcmc_out$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(countrynum = as.numeric(str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')))
# add country
countries = select(for_model, countrynum, country) %>%
  group_by(countrynum, country) %>%
  tally() %>%
  ungroup() 
table = left_join(table, countries, by='countrynum')

## add posterior p-values (two-sided)
pos = rbind(mcmc_out$samples$chain1, mcmc_out$samples$chain2) > 0
pos = colMeans(pos)
pos.dash = 1 - pos
pval = as.numeric(pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))) # 2* for two chains
table = bind_cols(table, pval)
names(table) = c('rowname','mean','median','sd','lower','upper','countrynum','country','number','pvalue')

## plot
to_plot = filter(table, 
                 pvalue < 0.01,
                 number > 100, # more than 100 reviews (otherwise driven by a few reviews)
                 str_detect(rowname, 'beta')) %>%
  arrange(mean) %>%
  mutate(x = 1:n(),
         label = paste(country, ' (n = ', number,')', sep=''),
         mean = exp(mean),
         lower = exp(lower),
         upper = exp(upper))
col = "olivedrab3"
cplot = ggplot(data = to_plot, aes(x = x, y = mean, ymin = lower, ymax = upper))+
  geom_point(col = col, size=2)+
  geom_hline(lty=2, yintercept=1)+
  geom_errorbar(width=0, col = col, linewidth=1.05)+
  scale_x_continuous(breaks = 1:nrow(to_plot), labels = to_plot$label)+
  scale_y_log10()+
  coord_flip()+
  xlab('')+
  ylab('Rate ratio')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())
cplot

# check mixing
plot(mcmc_out$samples$chain1[,1])
points(mcmc_out$samples$chain2[,1], col='red')
