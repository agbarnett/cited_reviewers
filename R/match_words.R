### function to match short affiliation in longer text
# called by find_institution

match_words = function(few_words, to_text_match){
  # remove punctuation
  few_words = str_remove_all(few_words, '\\.|,|;| of') # remove `of` as this is not an important match
  to_text_match = str_remove_all(to_text_match, '\\.|,|;')
  few_words = tolower(str_split(few_words, ' ')[[1]])
  to_text_match = tolower(str_split(to_text_match, ' ')[[1]])
  p_match = sum(few_words %in% to_text_match) / length(few_words)
  return(p_match)
}