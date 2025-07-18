# 7_interpret_reviews_with_LLM.R
# using LLM to interpret the peer reviewers' comments; run two models
# July 2025
library(officer) # for reading word
library(ellmer) # for interacting with LLMs
library(dplyr)
seed = TeachingDemos::char2seed('brackley')

# get review data from 5_match_papers_reviewers
load('data/5_analysis_data.RData')
# remove empty or very short reviews
matched = mutate(matched, nchar = nchar(rtext)) %>%
  filter(rtext > 300)

# get the prompt text from the Word document
prompt <- read_docx('prompt_f1000_agent.docx')
summary_paragraphs <- docx_summary(prompt)
prompt_text = paste(summary_paragraphs[summary_paragraphs$content_type %in% "paragraph", "text"], collapse='\n')

## to do: get results for two LLMs; pay for non-free AI
## take random sample of 10,000 rather than doing all reviews?
## to do, just self-cited and reject/reservations and then compare with others?
## run conditional logistic regression again?
# informative, clear, and actionable
# vague, unprofessional, unjustified

# for structured answers
my_schema <- type_object(vague = type_string(),
                         inappropriate = type_string())

# set up reviewer
meta_reviewer <- chat_google_gemini(system_prompt = prompt_text,
                          params = params(seed = seed, temperature=0))

## loop through reviews
llm_answers = NULL
for (k in 1:100){
  # run with structured answers
  outg <- meta_reviewer$chat_structured(matched$rtext[k],
                              type = my_schema)
  llm_answers = bind_rows(llm_answers, outg)
}



