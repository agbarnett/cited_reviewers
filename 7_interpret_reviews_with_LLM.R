# 7_interpret_reviews_with_LLM.R
# using LLM to interpret the peer reviewers' comments; run two models
# July 2025
library(officer) # for reading word
library(ellmer) # for interacting with LLMs
library(dplyr)
library(tidyr)
seed = TeachingDemos::char2seed('brackley')

# get review data from 5_match_papers_reviewers
load('data/5_analysis_data.RData')
# remove empty or very short reviews
matched = mutate(matched, nchar = nchar(rtext)) %>%
  filter(nchar > 300)

# get the prompt text from the Word document
prompt <- read_docx('prompt_f1000_agent.docx')
summary_paragraphs <- docx_summary(prompt)
prompt_text = paste(summary_paragraphs[summary_paragraphs$content_type %in% "paragraph", "text"], collapse='\n')

# for structured answers
my_schema <- type_object(vague = type_string(),
                         inappropriate = type_string())

# set up reviewer
meta_reviewer <- chat_google_gemini(system_prompt = prompt_text,
                          params = params(seed = seed, temperature=0))

## loop through reviews
llm_answers = NULL
start = 162 # for restarts
for (k in start:2500){
  # run with structured answers
  outg <- meta_reviewer$chat_structured(matched$rtext[k],
                              type = my_schema)
  frame = mutate(matched[k,],  # add DOI etc to LLM answer
                 llm_vague = outg$vague,
                 llm_inappropriate = outg$inappropriate)  
  llm_answers = bind_rows(llm_answers, frame)
}

# safety net for duplicates

# split llm answer and reasoning
llm_sep = '(?<=^no)(,|;) |(?<=^yes)(,|;) |(?<=^partly)(,|;) '
data = separate(llm_answers, col = 'llm_vague', into = c('llm_vague','llm_vague_reason'), sep=llm_sep)
data = separate(data, col = 'llm_inappropriate', into = c('llm_inappropriate','llm_inappropriate_reason'), sep=llm_sep)
table(data$llm_vague)
table(data$llm_inappropriate)
# need to remove some long answers

## take random sample of 10,000 rather than doing all reviews?
## run conditional logistic regression again?

save(llm_answers, data, file = 'data/7_reviews_llm.RData')

