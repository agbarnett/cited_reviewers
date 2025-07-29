# clogit_model.R
# function to run conditional logistic regression models
# June 2025

## function needed for countries for co-reviewers
mymode <- function(x) {
  t <- table(x)
  names(t)[ which.max(t) ]
}

## function to run one conditional logistic regression and store the results (used below)
run_one = function(formula, indata, this_v, this_co, this_o, this_c = '', this_type, alpha = 0.05){
  # option to change fit, not used
  my.control = coxph.control()
  #my.control$iter.max = 50
  #
  model = tryCatch(clogit(as.formula(formula), data = indata, method='exact', control = my.control), # needed trycatch for warnings about lack of convergence
                   warning = function(w){'warning'}) # 
  if(class(model)[1] == 'character'){
    s = data.frame(co_reviews = this_co,
                   version = this_v,
                   outcome = this_o,
                   confounder = this_c,
                   type = this_type,
                   iter = NA,
                   aic = NA,
                   warning = TRUE)
    predictions = NULL
  }
  if(class(model)[1] != 'character'){
    s = summary(model)
    s = data.frame(s$coefficients) %>% 
      tibble::rownames_to_column() %>%
      filter(rowname == 'pred') %>% # remove confounder
      mutate(co_reviews = this_co,
             version = this_v,
             outcome = this_o,
             confounder = this_c,
             type = this_type,
             iter = model$iter, # store iterations to check on model fit
             aic = AIC(model), 
             warning = FALSE) %>% # add model fit
      rename('p' = 'Pr...z..')
  
  ## predictions, only if model converged
  # see https://stackoverflow.com/questions/72140385/predicted-values-for-conditional-logistic-regression-greater-than-1
  # new dataframe with all potential strata
  newdat <- expand.grid(pred = 0:1, # just zero to 1
                        doi = strata(unique(indata$doi))) %>%
    mutate(works_count = 0, # not using confounders for predictions
           works_count_2 = 0,
           works_count_1 = 0,
           works_count_0 = 0,
           works_count2 = 0,
           works_count3 = 0)
  # predict
  pred_dat <- newdat %>%
    bind_cols(predict(model,
                      newdata = newdat,
                      type = "risk",
                      se.fit = TRUE))
  # generate probabilities and CIs
  predictions = mutate(pred_dat,
                       prob = fit / (1 + fit),
                       z = qnorm(1 - alpha/2), # /2 for two-sided ; using adjusted type I error
                       low = (fit - z * se.fit) / (1 + (fit - z * se.fit)),
                       up = (fit + z * se.fit) / (1 + (fit + z * se.fit)),
                       low = ifelse(low < 0, 0, low),
                       up = ifelse(up > 1, 1, up)) %>%
    group_by(pred) %>%
    summarise(prob = mean(prob), low = mean(low), up = mean(up)) %>% # take averages
    ungroup() %>%
    rename('upper' = 'up', 'lower' = 'low') %>%
    mutate(co_reviews = this_co, # add meta-data
           version = this_v,
           outcome = this_o,
           confounder = this_c,
           type = this_type)
  
  }
  
  #
  to_return = list()
  to_return$estimates = s
  to_return$predictions = predictions
  return(to_return)
}


## main function
run_clogit = function(indata, 
                      l1 = 'Approve vs\nReservations/Not approved', # short labels
                      l2 = 'Approve/Reservations\nvs Not approved',
                      co_reviews = TRUE, # include co-reviewers or not
                      countries, # for leave one country out analysis
                      predictor){
  
  ## type I error
  alpha = 0.05 # target type I error
  alpha_adj = alpha / (2*2*2) # 2 versions, linear vs any, accept/reservations/reject
  
  # filter by co_referee
  if(co_reviews == FALSE){
    indata = filter(indata, role == 'Referee') # just the main reviewer
  }
  if(co_reviews == TRUE){ # combine co-referees
    # get papers with co-referees
    co_refs = filter(indata, role != 'Referee') %>% select(doi, version, review_number) %>% unique()
    indata1 = anti_join(indata, co_refs, by = c('doi', 'version', 'review_number'))
    indata2 = left_join(co_refs, indata, by = c('doi', 'version', 'review_number'))
    # now get one recommendation per co-referee/referee combinations
    indata2_one = group_by(indata2, doi, version, review_number, recommendation) %>%
      summarise(n = n(), # just for checking
                works_count = sum(works_count), # total the reviewers' experience
                matches = sum(matches), # total of all citations to the reviewer
                self_cited_count = sum(matches), # total of all self-citations
                country = mymode(country) # use most common country
      ) %>% # 
      ungroup()
    indata = bind_rows(indata1, indata2_one)
  }
  
  # change name of predictor (must come after above ref vs co-ref)
  index = which(names(indata) == predictor)
  names(indata)[index] = 'predictor'
  
  # can't use if predictor is missing, e.g., assessment of matching could not be made
  indata = filter(indata, !is.na(predictor)) 
  
  # create fractional polynomials for confounder of reviewers' paper numbers
  indata = mutate(indata,
                  works_count = (works_count+0.5)/50, # scale to per 50 papers and remove zero
                  works_count_2 = works_count ^-2,
                  works_count_1 = works_count ^-1,
                  works_count_0 = log(works_count),
                  works_count2 = works_count ^2,
                  works_count3 = works_count ^3 # has some enormous outliers
  )
  
  # set up variations for loops
  confounders = c('', '+ works_count', '+ works_count_2', '+ works_count_1', '+ works_count_0', '+ works_count2', '+ works_count3')
  outcomes = c(" == 'approve'", " != 'reject'") # to make binary outcomes
  
  # first loop: outcome is linear or any vs none
  results = predictions = NULL
  for (pred_type in c('linear','any_vs_none')){
    
    # set up predictor
    indata = mutate(indata,
                    pred = case_when(
                      pred_type == 'linear' ~ predictor,
                      pred_type == 'any_vs_none' ~ predictor > 0
                    ))
    
    for (v in 1:2){
      # filter on article version number
      if(v==1){
        mdata = filter(indata, version==1) 
      }
      if(v==2){
        mdata = filter(indata, version!=1)
      }
      for (o in outcomes){
        
        # leave one country out sensitivity analysis
        formula = paste('I(recommendation', o , ') ~ pred + strata(doi)', sep='') # stratify by article and version
        for (this_country in countries){
          this_data = filter(mdata, country != this_country)
          s = run_one(formula = formula, indata = this_data, this_o = o, this_v = v, this_type = pred_type, this_co = co_reviews, this_c = this_country, alpha = alpha_adj)
          results = bind_rows(results, s$estimates)
          predictions = bind_rows(predictions, s$predictions)
        }
        
        # run through confounders with reviewer experience
        for (c in confounders){
          formula = paste('I(recommendation', o , ') ~ pred ', c, ' + strata(doi)', sep='') # stratify by article and version
          s = run_one(formula = formula, indata = mdata, this_o = o, this_v = v, this_type = pred_type, this_co = co_reviews, this_c = c, alpha = alpha_adj)
          results = bind_rows(results, s$estimates)
          predictions = bind_rows(predictions, s$predictions)
        }
      }
    }
    
  }
  
  ## adjustment to type I error for multiple hypothesis tests
  # prepare estimates for plot
  results = mutate(results, 
                   outcome = ifelse(outcome == outcomes[1], 1, 2), # change to number
                   Z = qnorm(1 - alpha_adj/2), # /2 for two-sided 
                   lower = coef - (Z*se.coef.), # confidence interval
                   upper = coef + (Z*se.coef.),
                   coef = exp(coef), # make into odds ratios
                   lower = exp(lower),
                   upper = exp(upper),
                   facet = ifelse(type == 'linear', "Linear citations", "None vs any citations"),
                   model = case_when( # needed for x-axis
                     version==1 & outcome==1 ~ 1,
                     version==1 & outcome==2 ~ 2,
                     version==2 & outcome==1 ~ 3,
                     version==2 & outcome==2 ~ 4
                   ),
                   clabel = case_when( # nicer label for confounding
                     confounder == 'country' ~ 'Country',
                     confounder == '' ~ 'None',
                     confounder == '+ works_count_2' ~ '-2',
                     confounder == '+ works_count_1' ~ '-1',
                     confounder == '+ works_count_0' ~ 'log',
                     confounder == '+ works_count2' ~ '2',
                     confounder == '+ works_count3' ~ '3',
                     confounder == '+ works_count' ~ '1'
                   )
  ) %>%
    select(-z, -Z, -'exp.coef.')
  row.names(results) = NULL
  
  # add nicer labels to predictions
  predictions = mutate(predictions, 
                   outcome = ifelse(outcome == outcomes[1], 1, 2), # change to number
                   facet = ifelse(type == 'linear', "Linear citations", "None vs any citations"),
                   clabel = case_when( # nicer label for confounding
                     confounder == 'country' ~ 'Country',
                     confounder == '' ~ 'None',
                     confounder == '+ works_count_2' ~ '-2',
                     confounder == '+ works_count_1' ~ '-1',
                     confounder == '+ works_count_0' ~ 'log',
                     confounder == '+ works_count2' ~ '2',
                     confounder == '+ works_count3' ~ '3',
                     confounder == '+ works_count' ~ '1'
                   )
  ) 
  row.names(predictions) = NULL

  # make the plot
  vcolours = c('darkseagreen3','dodgerblue')
  # text
  text1 = data.frame(model = 0.75, y = 1, lower = 1, upper = 1, version=1, outcome=1, label = 'More favourable\nrecommendation') 
  text2 = data.frame(model = 0.75, y = 1, lower = 1, upper = 1, version=1, outcome=1, label = 'Less favourable\nrecommendation') 
  # text inside plot to label outcomes
  itext1 = data.frame(model = 1.5, y = 1, lower = NA, upper = NA, version = 1, outcome = 1,
                      label = 'Version 1')
  itext2 = data.frame(model = 3.5, y = 1, lower = NA, upper = NA, version = 1, outcome = 1,
                      label = 'Version 2+')
  #
  for_plot = filter(results, !confounder %in% countries) # remove country results
  for_plot = filter(for_plot, str_detect(confounder, pattern = '0$')) # plot results for log-transformed works count as that was generally the best fit, see AIC below; although does not really matter, see eplot
  # odds ratio label
  ylab = "Odds ratio (Approve \u2192 Reservations \u2192 Not approved)" # using symbol for the right arrow
  #
  plot = ggplot(data = for_plot, aes(x = model, y = coef, ymin=lower, ymax = upper, 
                                     col=factor(outcome)))+
    geom_hline(yintercept=1, lty=2, col='grey33')+
    geom_vline(xintercept=2.5, lty=1, col='grey66')+ # split outcomes
    geom_point(size=5)+
    geom_errorbar(width=0, linewidth=1.1)+
    geom_text(data = text1, aes(x=model, y=y, label=label), size=3, adj=-0.1, col='grey66')+
    geom_text(data = text2, aes(x=model, y=y, label=label), size=3, adj=1.1, col='grey66')+
    geom_label(data = itext1, aes(x=model, y=y, label=label), size=3, col='grey33', fill ='white')+
    geom_label(data = itext2, aes(x=model, y=y, label=label), size=3, col='grey33', fill ='white')+
    xlab('')+
    ylab(ylab)+
    scale_color_manual('Recommendation', values = vcolours, labels=c(l1, l2))+
    scale_x_reverse(breaks=1:4, labels=NULL, limits=c(4.25,0.75))+ # put version 1 higher
    scale_y_log10() +
    theme_bw()+
    theme(axis.ticks.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key.width = unit(36, 'pt'), # make key wider to show CIs
          legend.key.spacing.y = unit(8, "pt"))+ # increase space in legend because of long text
    coord_flip()+
    facet_wrap(~facet, scales='free')
  plot
  
  # return table and plot
  to_return = list()
  results = select(results, -rowname) # not needed
  to_return$results = results
  to_return$predictions = predictions
  to_return$plot = plot
  return(to_return)
  
}
