# 7_text_differences_reviews.R
# using text analysis to look for differences in reviews with self-citations or not
# September 2025
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

source('7_text_prepare.R')

# create the predictor matrix for elastic net
x = matrix(data = 0, nrow = nrow(matched), ncol = 100)
for (k in 1:100){ # takes a while
  this_feature = paste('\\b', top_100$feature[k], '([a-z]{1,10})?\\b', sep='') # needs to be words only; start from stem and end at next break with optional additional letters
  x[,k] = str_detect(matched$rtext, this_feature) 
}
summary(x); any(colMeans(x)==0) # checks
y = matched$self_cited_count > 0 # binary outcome of any self-citation

# run the elastic net
fit = glmnet(x, y, family='gaussian', alpha = 0.95) # data is large enough to use Gaussian
cvfit = cv.glmnet(x, y, family='gaussian', alpha = 0.95)
plot(cvfit)
c1 = coef(cvfit, s = "lambda.1se")
# extract non-zero coefficients
to_plot = data.frame(vars = top_100$feature[c1@i], coefs = c1@x[-1]) %>% # not intercept
  filter(abs(coefs) > 0.001) %>% # remove very small estimates
  arrange(coefs) %>%
  mutate(x = 1:n())

## plots
# plot the estimates
plot = ggplot(data = to_plot, aes(x = x, y = coefs)) +
  ggtitle('Probability of self-citation')+
  geom_point(col='darkseagreen4')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = 1:nrow(to_plot), labels = to_plot$vars)+
  xlab(NULL)+
  ylab('Difference in probability')+
  coord_flip()
plot 