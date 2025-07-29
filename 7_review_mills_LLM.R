# 7_review_mills_LLM.R
# using LLM to find review mills
# see https://link.springer.com/article/10.1007/s11192-024-05125-w
# July 2025
library(officer) # for reading word
library(ellmer) # for interacting with LLMs
library(dplyr)
library(tidyr)
seed = TeachingDemos::char2seed('birmingham')

# get review data from 5_match_papers_reviewers
load('data/5_analysis_data.RData')
# remove empty or very short reviews
matched = mutate(matched, nchar = nchar(rtext)) %>%
  filter(rtext > 300)

# get the prompt text from the Word document
prompt <- read_docx('prompt_f1000_agent_mills.docx')
summary_paragraphs <- docx_summary(prompt)
prompt_text = paste(summary_paragraphs[summary_paragraphs$content_type %in% "paragraph", "text"], collapse='\n')

# for structured answers
my_schema <- type_object(vague = type_string())

# set up reviewer
meta_reviewer <- chat_google_gemini(system_prompt = prompt_text,
                          params = params(seed = seed, temperature=0))

## loop through reviews
llm_answers = NULL
start = 1 # for re-starts
for (k in start:25){ # big loop
  # run with structured answers
  outg <- meta_reviewer$chat_structured(matched$rtext[k],
                              type = my_schema)
  frame = mutate(matched[k,], llm = outg$vague) # add DOI etc to LLM answers 
  llm_answers = bind_rows(llm_answers, frame)
}
# split llm answer and reasoning
llm_sep = '(?<=^no)(,|;) |(?<=^yes)(,|;) |(?<=^partly)(,|;) '
llm_answersx = separate(llm_answers, col = 'llm', into = c('llm_answer','llm_reason'), sep=llm_sep)
# get warning where there's no sentence
table(llm_answersx$llm_answer) # check of frequencies

# not working well

# save
save(llm_answers, file = 'data/7_review_mills.RData')


