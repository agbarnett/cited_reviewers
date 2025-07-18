# 1_report_search.R
# report search results
# March 2025
library(rmarkdown)

render(input = "1_search_results.Rmd",
       output_format = 'word_document',
       output_dir = 'results', # into separate folder
       output_file = "1_search_results.docx")
