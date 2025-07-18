# 6_random_checks.R
# random checks to verify that analysis data set is accurate
# make the random selections and create an Excel file
# May 2025
library(flextable)
library(dplyr)
library(openxlsx)
TeachingDemos::char2seed('andorra')

## get the data
# from 5_match_papers_reviewers.R
load('data/5_analysis_data.RData')
# number to randomly sample
n_sample = 40

## random selections
# papers where reviewer is not cited in the paper's references
rselect1 = filter(matched, matches == 0) %>%
  sample_n(n_sample) %>%
  select(doi, name, n_papers_cited)
## papers where reviewer is cited in the paper's references
rselect2 = filter(matched, matches > 0) %>%
  sample_n(n_sample) %>%
  select(doi, name, n_papers_cited, matches)
## reviews with self-citations
rselect3 = filter(matched, self_cited_count > 0) %>%
  sample_n(n_sample) %>%
  select(doi, name, self_cited_count)


## export to excel - make sure to rename excel file after data collection
header_style <- createStyle(fontColour = "white", fgFill = "darkorange2", halign = "left", valign = "center", wrapText = TRUE, textDecoration = "Bold", border = "TopBottomLeftRight") # 
#
wb <- createWorkbook(title = 'Random checks', creator = 'Adrian Barnett', subject = 'created by 6_random_checks.R')
# sheet 1
rselect1$checked = '' # empty column to complete
addWorksheet(wb, sheetName = 'Not cited', gridLines = TRUE)
freezePane(wb, sheet = 'Not cited', firstRow = TRUE)
writeData(wb, sheet = 'Not cited', x = rselect1, startRow=1, headerStyle = header_style)
setColWidths(wb, sheet = 'Not cited', cols=1:4, widths=c(31,25,8,8))
# sheet 2
rselect2$checked = '' # empty column to complete
addWorksheet(wb, sheetName = 'Reviewer cited', gridLines = TRUE)
freezePane(wb, sheet = 'Reviewer cited', firstRow = TRUE)
writeData(wb, sheet = 'Reviewer cited', x = rselect2, startRow=1, headerStyle = header_style)
setColWidths(wb, sheet = 'Reviewer cited', cols=1:5, widths=c(31,25,8,8,8))
# sheet 3
rselect3$checked = '' # empty column to complete
addWorksheet(wb, sheetName = 'Self cited', gridLines = TRUE)
freezePane(wb, sheet = 'Self cited', firstRow = TRUE)
writeData(wb, sheet = 'Self cited', x = rselect3, startRow=1, headerStyle = header_style)
setColWidths(wb, sheet = 'Self cited', cols=1:4, widths=c(31,25,8,8))
# save
saveWorkbook(wb, "checks/6_random_checks.xlsx", overwrite = TRUE)

