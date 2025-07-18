# 3_random_checks.R
# random checks that XML reading has worked
# March 2025
library(rmarkdown)

render(input = "3_random_checks.Rmd",
       output_format = 'word_document',
       output_dir = 'checks', # into separate folder
       output_file = "3_random_checks_xml.docx")

