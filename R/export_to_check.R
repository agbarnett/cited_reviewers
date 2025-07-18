
## exporting reviewer and matched affiliations for manual checks
export_to_check_file = function(reviewer, match){
  load('checks/2_check_author_match.RData') # from find_author.R
  today = as.Date(Sys.Date())
  num = round(runif(n=1, min=0, max=1000000)) # random ID number for matching in later checking file
  c1 = data.frame(num = num, date = today, affiliation = reviewer$affiliation)
  checks1 = bind_rows(checks1, c1)
  c2 = data.frame(num = num, date = today, affiliation = match$affiliation_display_name)
  checks2 = bind_rows(checks2, c2)
  save(today, checks1, checks2, readme, file = 'checks/2_check_author_match.RData')
}