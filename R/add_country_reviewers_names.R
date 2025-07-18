# add_country_reviewers_names.R
# add reviewers' country
# July 2024

add_country_reviewers_names = function(reviewers_names){
  
  # add reviewers' country and country code from affiliation
  reviewers_names = mutate(reviewers_names,
                           country = add_country(affiliation)) # see 99_functions
  
  # hard code one missing country (typo for 'Swaziland')
  index = reviewers_names$doi == '10.12688/wellcomeopenres.23042.1' & is.na(reviewers_names$country)
  reviewers_names$country[index] = 'Switzerland'
    
  # add country code for later merging
  # data from https://github.com/agbarnett/ISO-3166-Countries-with-Regional-Codes/tree/master/all  
  countries = read.csv('data/all_countries.csv', na.strings = '') %>%  # need na strings because of Namibia = NA
    clean_names() %>%
    select(name, alpha_2) %>%
    rename('country' = 'name', 'country_code' = 'alpha_2')
  reviewers_names = left_join(reviewers_names, countries, by='country')
  
  # tidy up names
  reviewers_names = mutate(reviewers_names,
                           affiliation = str_squish(affiliation)) # 
  
  # 
  return(reviewers_names)
}
