# 7_report_checks.R
# report on the random checks
# June 2025
library(rmarkdown)

render(input = "7_random_checks.Rmd",
       output_format = 'word_document',
       output_dir = 'checks', # into separate results directory
       output_file = "7_random_checks.docx")
