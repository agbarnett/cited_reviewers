# 7_text_prepare.R
# prepare the data for the text analysis, called by 7_text_difference_reviews...
# September 2025

# get review data from 5_match_papers_reviewers
load('data/5_analysis_data.RData')

# remove empty or very short reviews
matched = mutate(matched, nchar = nchar(rtext)) %>%
  filter(nchar > 100) # longer than a sentence

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

# ngrams of size 2; top 100 ngrams
toks_ngram <- tokens_ngrams(tokens, n = 2)
doc_ngram = dfm(toks_ngram)
top_100_ngram = textstat_frequency(doc_ngram, n = 100)

# make tm corpus for unstemming
all_words = pull(matched, rtext)
tm_corpus = VCorpus(VectorSource(all_words))

# unstemmed version of top 100 words for plots
unstemmed = rep('', 100) 
for (k in 1:100){ # takes a while
  unstemmed[k] = stemCompletion(top_100$feature[k], tm_corpus, type='prevalent') # takes most frequent (is slow)
}
