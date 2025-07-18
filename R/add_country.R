# add_country.R
# add country from affiliation
# July 2024

add_country = function(text){
  countries = read.csv('data/all_countries.csv') %>% # from https://github.com/agbarnett/ISO-3166-Countries-with-Regional-Codes/tree/master/all
    clean_names() %>%
    pull(name)
  search = paste(paste('\\b', paste(countries, collapse='\\b|\\b'), sep=''), '\\b', sep='')
  country = stringi::stri_extract_last(str = text, regex = search) # take last country, e.g., 'Geisel School of Medicine at Dartmouth, Lebanon, NH, USA'
  # consolidate some countries
  country = ifelse(country == 'Brunei', "Brunei Darussalam", country)
  country = ifelse(country == "Cote d'Ivoire", "Côte d'Ivoire", country)
  country = ifelse(country == "UK", "United Kingdom", country)
  return(country)
}
