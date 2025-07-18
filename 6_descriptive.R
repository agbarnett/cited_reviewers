# 6_descriptive.R
# descriptive statistics
# June 2025
library(rmarkdown)

render(input = "6_descriptive.Rmd",
       output_format = 'word_document',
       output_dir = 'results', # into separate results directory
       output_file = "6_descriptive.docx")