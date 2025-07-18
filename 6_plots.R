# 6_plots.R
# plots of self-citations and other summary plots; takes a while
# May 2025
library(rmarkdown)

render(input = "6_plots.Rmd",
       output_format = 'word_document',
       output_dir = 'results', # into separate results directory
       output_file = "6_plots.docx")