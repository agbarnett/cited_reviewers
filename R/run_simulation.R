# run_simulation.R
# function to run simulation for ordinal outcome
# used for sample size calculation.

run_simulation_ordinal = function(
    alpha = 0.8, # effect of being cited on latent assessment of quality (logit scale)
    n_papers = 1000,
    reviewers_per_paper = 2,
    u1 = FALSE, # unmeasured confounding -> quality and -> citation
    latent_thresholds = c(-3.2,-1), # latent thresholds to make categories (aiming for 67:30:3)
    p_cited = 0.15, # probability reviewer is cited
    sd_quality = 1){ # standard deviation in article quality
  
  # difference in probability
  true = prob(0+alpha) - prob(0)
  
  # create quality dependent on unmeasured confounding
  unmeasured1 = rnorm(n = n_papers, mean = 0, sd = sd_quality) # use same SD as per quality
  q = rnorm(n = n_papers, mean = 0, sd = sd_quality) # latent quality
  quality = q + as.numeric(u1)*unmeasured1 # add if u1 is true
  
  # create an initial data frame
  sim_data = NULL
  n_reviewers = 2 + rbinom(n = n_papers, size = 1, prob = 0.2) # number of reviewers, most often 2, sometimes 3
  for (k in 1:n_papers){
    for (j in 1:n_reviewers[k]){
      frame = data.frame(paper = k, 
                         reviewer = j, 
                         quality = quality[k],
                         unmeasured1 = unmeasured1[k])
      sim_data = bind_rows(sim_data, frame)
    }
  }
  
  # create the exposure data and binary assessment
  sim_data = mutate(sim_data, 
                    latent_cited = inv.prob(p_cited) + as.numeric(u1)*unmeasured1, # regression equation for chance of exposure (reviewer cited)
                    cited = rbinom(n=n(), size=1, prob=prob(latent_cited)), # cited or not - randomly generated
                    rating = quality + cited*alpha, # latent assessment of quality
                    #pr = rep(NA,3),
                    pr_3 = prob(rating + latent_thresholds[1]), # create probability from regression equation for highest category
                    pr_2 = prob(rating + latent_thresholds[2]) - pr_3, # create probability from regression equation for mid category
                    pr_1 = 1 - (pr_2+pr_3),
                    recommendation = NA)
  # individual loop for simulating ordinal outcome
  for (k in 1:nrow(sim_data)){
    probs = c(sim_data$pr_1[k], sim_data$pr_2[k], sim_data$pr_3[k])
    sim_data$recommendation[k] = sum(nimble::rmulti(n=1, size=1, prob = probs)*1:3)
  }
  
  # run models
  model1 = clogit(I(recommendation>1) ~ cited + strata(paper), data = sim_data) # revise & accept vs reject
  model2 = clogit(I(recommendation>2) ~ cited + strata(paper), data = sim_data) # accept vs revise & reject
  #s = summary(model)
  ests1 = tidy(model1, conf.int=TRUE, conf.level = 0.975) %>% # confidence level adjust for multiple comparisons
    mutate(true = true,
           alpha = alpha, # add meta data
           p_cited = p_cited,
           sd_quality = sd_quality,
           u1 = u1) %>%
    select(-term) # not needed
  ests2 = tidy(model2, conf.int=TRUE, conf.level = 0.975) %>%
    mutate(true = true,
           alpha = alpha, # add meta data
           p_cited = p_cited,
           sd_quality = sd_quality,
           u1 = u1) %>%
    select(-term) # not needed
  ests = bind_rows(ests1, ests2, .id = 'model')
  
  # return
  to.return = list()
  to.return$clogit = ests
  return(to.return)
  
} # end of function

