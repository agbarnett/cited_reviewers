# 99_reviewers_comments.R
# make table of reviewers' comments for paper
# May 2025
library(dplyr)
library(xtable) # for latex
library(textclean)

# had to export to csv as read_xls did not work
comments = read.table('checks/6_random_checks_complete.txt', header=TRUE, fill=TRUE, sep='\t', quote='') %>%
  mutate(what.reviewer.said = replace_non_ascii(what.reviewer.said),
         nch = nchar(what.reviewer.said),
         nch = ifelse(what.reviewer.said=='No relevant sentence', 0, nch)) %>%
  arrange(desc(nch)) %>%
  select(self_cited_count, what.reviewer.said)

#
print(xtable(comments), 
      include.rownames=FALSE, 
      hline.after=FALSE, 
      only.contents = TRUE,
      file = "results/99_reviewer_comments.tex")

