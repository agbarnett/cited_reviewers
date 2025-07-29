# 7_text_differences_reviews.R
# using text analysis to look for differences in reviews with self-citations or not
# July 2025
library(dplyr)
library(stringr)
library(tidytext) # for stop words
library(tm) # for unstemming words
library(quanteda) # for corpus
library(quanteda.textstats) # for word counts
library(glmnet) # for elastic net
library(ggplot2)
library(gridExtra)
seed = TeachingDemos::char2seed('birmingham')

# get review data from 5_match_papers_reviewers
load('data/5_analysis_data.RData')

# remove empty or very short reviews
matched = mutate(matched, nchar = nchar(rtext)) %>%
  filter(rtext > 300)

# tidy the review text
number_pattern = paste(paste(' ', 1:30, ' ', sep=''), collapse='|')
year_pattern = ' [1-2][0-9][0-9][0-9] '
matched = mutate(matched, 
                 rtext = str_to_lower(rtext),
                 rtext = str_remove_all(rtext, "[^[:alnum:][:space:]]"), # remove non letters and numbers
                 rtext = str_replace_all(rtext, 'et.al', 'et-al'), # change etal to one word for analysis
                 rtext = str_replace_all(rtext, pattern = number_pattern, replace = ' '), # remove numbers
                 rtext = str_replace_all(rtext, pattern = number_pattern, replace = ' ') # remove years
)

# find the 100 most common words in all reviews
all_words = pull(matched, rtext) %>% paste(collapse = '. ')
all_words = corpus(all_words)
tokens = tokens(all_words) # takes a little while
tokens = tokens_remove(tokens, pattern = stopwords("en"))
tokens = tokens_wordstem(tokens) # stem the words, remove plurals
tokens = tokens_select(tokens, c("."), selection = "remove", padding = FALSE) # remove full stop
doc = dfm(tokens)
top_100 = textstat_frequency(doc, n = 100)

# make tm corpus for unstemming
all_words = pull(matched, rtext)
tm_corpus = VCorpus(VectorSource(all_words))


# create predictor matrix for elastic net
x = matrix(data = 0, nrow = nrow(matched), ncol = 100)
unstemmed = rep('', 100) # unstemmed version of top 100 words; just need to create once
for (k in 1:100){
  this_feature = paste('\\b', top_100$feature[k], '([a-z]{1,10})?\\b', sep='') # needs to be words only; start from stem and end at next break with optional additional letters
  x[,k] = str_detect(matched$rtext, this_feature) 
  unstemmed[k] = stemCompletion(top_100$feature[k], tm_corpus, type='prevalent') # takes most frequent (is slow)
}
summary(x); any(colMeans(x)==0) # checks
y = matched$self_cited_count > 0 # binary outcome of any self-citation
# run elastic net
fit = glmnet(x, y, family='gaussian', alpha = 0.95) # data is large enough to use Gaussian
cvfit = cv.glmnet(x, y, family='gaussian', alpha = 0.95)
plot(cvfit)
c1 = coef(cvfit, s = "lambda.1se")
# extract non-zero coefficients
to_plot1 = data.frame(vars = top_100$feature[c1@i], coefs = c1@x[-1]) %>% # not intercept
  filter(abs(coefs) > 0.001) %>% # remove very small estimates
  arrange(coefs) %>%
  mutate(x = 1:n())

## second analysis: subset of self cites, with not approved as outcome
matched_self = filter(matched, self_cited_count > 0)
# same x design as above, but in subset
x = matrix(data = 0, nrow = nrow(matched_self), ncol = 100)
for (k in 1:100){
  this_feature = paste('\\b', top_100$feature[k], '[a-z]?.*\\b', sep='') # needs to be words only; start from stem and end at next break with optional additional letters
  x[,k] = str_detect(matched_self$rtext, this_feature) 
}
summary(x); any(colMeans(x)==0) # checks
y = matched_self$recommendation != 'approve' # binary outcome not approved
# run elastic net
fit = glmnet(x, y, family='gaussian', alpha = 0.95) # data is large enough to use Gaussian
cvfit = cv.glmnet(x, y, family='gaussian', alpha = 0.95)
plot(cvfit)
c2 = coef(cvfit, s = "lambda.1se")
# extract non-zero coefficients
to_plot2 = data.frame(vars = top_100$feature[c2@i], coefs = c2@x[-1]) %>% # not intercept
  filter(abs(coefs) > 0.001) %>% # remove very small estimates
  arrange(coefs) %>%
  mutate(x = 1:n())

## ngrams of size 2
# top 100 ngrams
toks_ngram <- tokens_ngrams(tokens, n = 2)
doc_ngram = dfm(toks_ngram)
top_100_ngram = textstat_frequency(doc_ngram, n = 100)

## plots
# plot the estimates
plot1 = ggplot(data = to_plot1, aes(x = x, y = coefs)) +
  ggtitle('Probability of self-citation')+
  geom_point(col='darkseagreen4')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = 1:nrow(to_plot1), labels = to_plot1$vars)+
  xlab(NULL)+
  ylab('Difference in probability')+
  coord_flip()
# text
text1 = data.frame(x = nrow(to_plot2) - 1.2, y = 0.005, lower=0, upper=0, label = 'Favours\nReservations\nor Not approved') 
text2 = data.frame(x = nrow(to_plot2) - 1.2, y = -0.005, lower=0, upper=0, label = 'Favours\nApproved') 
#
plot2 = ggplot(data = to_plot2, aes(x = x, y = coefs)) +
  ggtitle('Probability of Not approved')+
  geom_point(col='darkorange2')+
  geom_hline(lty=2, yintercept=0)+
  geom_label(data = text1, aes(x=x, y=y, label=label), size=2.5, adj=0, col='grey50')+
  geom_label(data = text2, aes(x=x, y=y, label=label), size=2.5, adj=1, col='grey50')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = 1:nrow(to_plot2), labels = to_plot2$vars, expand=c(0.02,0.02))+
  xlab(NULL)+
  ylab('Difference in the probability of the reviewer\nrecommending Reservations or Not approved')+
  coord_flip()

# export
jpeg('figures/7_text_differences.jpg', width = 4, height = 6, units = 'in', res = 500)
grid.arrange(plot1, plot2, ncol = 1, heights=c(1,2.3))
dev.off()

# export
plot2 = plot2 + ggtitle(NULL)
jpeg('figures/7_text_differences_approve.jpg', width = 4, height = 5, units = 'in', res = 500)
print(plot2)
dev.off()

## randomly select sentences with please, only for those who self-cited and did not approve ##
please = filter(matched, 
                recommendation != 'approve',
                self_cited_count > 0,
                str_detect(tolower(rtext), 'please'))
nrow(please)
# random sample and extract sentence
TeachingDemos::char2seed('italy')
please_sample = sample_n(please, 20) %>%
  mutate(rtext = str_replace_all(rtext, 'vs\\.','vs'), # remove some non full-stops
         rtext = str_replace_all(rtext, 'et.al\\.','et-al')) %>%
  pull(rtext)
sentences = NULL
for (k in 1:20){
  split = str_split(please_sample[k], pattern = '\\. |\\? |\\! ')[[1]]
  index = str_detect(tolower(split), 'please')
  this = paste(split[index], collapse='. ') 
  sentences = c(sentences, this)
}  


#### part 2: run Bayesian model to get uncertainty intervals ####
library(nimble)

## only use words (x variables) selected by elastic net
columns_selected = which(top_100$feature %in% to_plot2$vars)
x_selected = x[,columns_selected]
## define the model
code <- nimbleCode({
  for (i in 1:N) {
    cite[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + inprod(gamma[1:M], x[i,1:M])
  }
  # 
  for(j in 1:M){
    gamma[j] ~ dt(mu = 0, sigma = 0.011, df=3) # tight prior to mimic elastic net (using trial and error)
  }
  alpha ~ dnorm(0, sd = 100)
  tau ~ dgamma(0.1, 0.1)
})

## data
constants <- list(N = nrow(x_selected),
                  M = ncol(x_selected),
                  x = x_selected)
data <- list(cite = y)

## initial values
inits <- list(tau = 1,
              gamma = rep(0, ncol(x_selected)),
              alpha = mean(y))

# parameters to store
parms = c('alpha','gamma','tau')

# models
model <- nimbleModel(code, 
                     data = data, 
                     inits = inits, 
                     constants = constants)

# MCMC samples
MCMC = 5000; thin = 5; n.chains = 2
seeds = rep(0,2)
seeds[1] = TeachingDemos::char2seed('wigan')
seeds[2] = TeachingDemos::char2seed('bradford')
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

## extract summary
table = as.data.frame(mcmc_out$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|_|\\[|\\]'),
         index = as.numeric(index))

## add posterior p-values for no difference
pos = rbind(mcmc_out$samples$chain1, mcmc_out$samples$chain2) > 0
pos = colMeans(pos)
pos.dash = 1 - pos
pval = pmax(2*pmin(pos, pos.dash), 1/(n.chains*MCMC)) # two-sided; 
table = bind_cols(table, pval)
names(table) = c('rowname','mean','median','sd','lower','upper','index','pvalue')

## make a combined plot with lasso and bayes results
bayes = filter(table, str_detect(rowname, 'gamma'))
frame = data.frame(vars = top_100$feature[columns_selected], # stemmed and ... 
                   unstem = unstemmed[columns_selected], #  ... unstemmed
                   index = 1:length(columns_selected))
bayes = left_join(bayes, frame, by='index')
to_plot3 = left_join(to_plot2, bayes, by='vars') %>%
  mutate(vars_plus = paste(vars, ' (', unstem, ')', sep = '')) # y-axis with labels and unstemmed
# text
text1 = data.frame(x = 3, y = 0.007, lower=0, upper=0, label = 'Favours\nReservations\nor Not approved') 
text2 = data.frame(x = nrow(to_plot2) - 1.2, y = -0.007, lower=0, upper=0, label = 'Favours\nApproved') 
# plot
plot3 = ggplot(data = to_plot3, aes(x = x, y = mean, ymin=lower, ymax=upper)) +
  geom_point(data = to_plot3, aes(x=x, y=coefs), pch=1, col='black', size=2)+
  geom_errorbar(width=0,col='darkorange2')+
  geom_point(col='darkorange2')+
  geom_hline(lty=2, yintercept=0)+
  geom_label(data = text1, aes(x=x, y=y, label=label), size=2.5, adj=0, col='grey50')+
  geom_label(data = text2, aes(x=x, y=y, label=label), size=2.5, adj=1, col='grey50')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = 1:nrow(to_plot3), labels = to_plot3$vars_plus, expand=c(0.02,0.02))+
  xlab(NULL)+
  ylab('Difference in the probability of the reviewer\nrecommending Reservations or Not approved')+
  coord_flip()
plot3
# export
jpeg('figures/7_text_differences_approve_plus.jpg', width = 5, height = 5, units = 'in', res = 500)
print(plot3)
dev.off()

# check chain
plot(mcmc_out$samples$chain1[,1])
acf(mcmc_out$samples$chain1[,1])
