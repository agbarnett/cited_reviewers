# 7_text_differences_reviews.R
# using text analysis to look for differences in reviews with self-citations or not
# July 2025
library(dplyr)
library(stringr)
library(tidytext) # for stop words
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
doc = dfm(tokens)
top_100 = textstat_frequency(doc, n = 100)

# create predictor matrix for elastic net
x = matrix(data = 0, nrow = nrow(matched), ncol = 100)
for (k in 1:100){
  x[,k] = str_detect(matched$rtext, top_100$feature[k]) # needs to be words only
}
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
# create predictor matrix for elastic net
x = matrix(data = 0, nrow = nrow(matched_self), ncol = 100)
for (k in 1:100){
  x[,k] = str_detect(matched_self$rtext, top_100$feature[k]) # needs to be words only
}
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
text1 = data.frame(x = nrow(to_plot2) - 1.2, y = 0.005, label = 'Favours\nreservations\nor reject') 
text2 = data.frame(x = nrow(to_plot2) - 1.2, y = -0.005, label = 'Favours\napprove') 
#
plot2 = ggplot(data = to_plot2, aes(x = x, y = coefs)) +
  ggtitle('Probability of not approving')+
  geom_point(col='darkorange2')+
  geom_hline(lty=2, yintercept=0)+
  geom_label(data = text1, aes(x=x, y=y, label=label), size=2.5, adj=0, col='grey55')+
  geom_label(data = text2, aes(x=x, y=y, label=label), size=2.5, adj=1, col='grey55')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = 1:nrow(to_plot2), labels = to_plot2$vars, expand=c(0.02,0.02))+
  xlab(NULL)+
  ylab('Difference in probability of recommending\nreservations or reject')+
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

## randomly select sentences with please, only for those who self-cited and did not approve
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

