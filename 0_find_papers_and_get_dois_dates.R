# 0_find_papers_and_get_dois_dates.R
# find potential papers using journal APIs and get DOIs
# version using dates; slightly better than version using types (as returned more papers)
# see https://f1000research.com/developers
# API warning: users can only make 100 requests per 60 seconds; There is a limit of 100 results per request.
# May 2025
library(dplyr)
library(stringr)
library(lubridate)
library(xml2)

## four study journals here; see list here: https://www.f1000.com/researchers/; 
# include API pages for the four journals for search queries
study_journals = read.table(header=TRUE, sep='!', text='
num!journal!start_date!api_start
1!F1000Research!2012-01-01!https://f1000research.com/extapi/search?q=
2!Wellcome Open Research!2016-11-01!https://wellcomeopenresearch.org/extapi/search?q=
3!Gates Open Research!2017-01-01!https://gatesopenresearch.org/extapi/search?q=
4!Open Research Europe!2022-03-01!https://open-research-europe.ec.europa.eu/extapi/search?q='
) %>%
  mutate(start_date = as_date(start_date))

# start and end dates
earliest_date = min(study_journals$start_date)
search_date = as.Date('2025-05-28') # record date that search was done 

# big loop to run through dates, loop in days to avoid multiple XML pages
all_dois = NULL
request = 0
request_time = Sys.time()
for(this_date in earliest_date:search_date){ # move in days
  
  # dates as numbers
  this_date_as_date = as.Date(this_date) # need to be a date not a number
  start_datetime = as.POSIXct(paste(this_date_as_date, '00:00:00'), format = '%Y-%m-%d %H:%M:%S', tz='UTC') 
  start_datetime_mill = as.numeric(start_datetime)*1000 # as a number, x1000 for milliseconds
  start_datetime_mill = format(start_datetime_mill, scientific = FALSE) # cannot be in scientific format
  end_datetime = as.POSIXct(paste(this_date_as_date, '23:59:59'), format = '%Y-%m-%d %H:%M:%S', tz='UTC') # very end of the same day
  end_datetime_mill = as.numeric(end_datetime)*1000
  end_datetime_mill = format(end_datetime_mill, scientific = FALSE)
  # build query using encoding (https://www.w3schools.com/tags/ref_urlencode.ASP)
  date_query = paste("R_PUD:%5B", start_datetime_mill, '%20TO%20', end_datetime_mill, '%5D', sep='')
  date_query_recent = paste("R_LU:%5B", start_datetime_mill, '%20TO%20', end_datetime_mill, '%5D', sep='')
  
  # loop through journals
  for (j in 1:4){
    # skip if query date is before journal start date
    j_start_date = filter(study_journals, num == j) %>% pull(start_date)
    if(this_date < j_start_date){next}
    
    # run query
    url_start = filter(study_journals, num == j) %>% pull(api_start)
    url = paste(url_start, date_query, sep='')
    destfile = 'api_query.xml'
    download_xml(url = url, file = destfile) # run the query and download the result
    request = request + 1 # count the number of requests
    Sys.sleep(0.2) # avoid time-outs from APIs (limit is 100 per minute)
    
    # read in the XML and extract the DOIs
    query_result = read_xml(destfile, encoding='UTF-8')
    file.remove(destfile) # clean up to avoid file being read in for next query
    dois = xml_text(xml_find_all(query_result, '//doi')) # get the DOIs
    if(length(dois) == 0){next} # skip to next
    if(length(dois) > 100){
      cat(paste('Warning, more that 100 DOIs for date = ', this_date,'\n', sep=''))
    }
    # concatenate
    to_bind = data.frame(journal = filter(study_journals, num == j) %>% pull(journal), date = this_date_as_date, doi = dois)
    all_dois = bind_rows(all_dois, to_bind)
  } # end of journal loop
  
  # output occasional update on time and requests
  if(runif(1)<0.01){
    time_diff = difftime(Sys.time(), request_time, units='mins')
    cat('Requests made = ', request, ', minutes spent = ', time_diff, ', ', round(request/as.numeric(time_diff)), ' requests per minute.\r', sep ='')
    cat('Up to date = ', this_date_as_date, '.\r')
  }
  
}

## search only gives latest version, so get all earlier versions
all_dois = mutate(all_dois, version = as.numeric(str_extract(doi, '[1-9][0-9]?$'))) # make numeric version
versions = filter(all_dois, version>1) %>% select(version) %>% unique() %>% pull
for (v in versions){
  this_version = filter(all_dois, version == v)
  for (previous in 1:(v-1)){ # loop through all previous versions
    old = paste(v, '$', sep='')
    new_version = mutate(this_version,
                         doi = str_replace(doi, old, as.character(previous)),
                         version = previous)
    all_dois = bind_rows(all_dois, new_version)
  }
}
# remove small number of duplicates
check = get_dupes(all_dois) # function from janitor
all_dois = unique(all_dois) %>% 
  select(-date) # no longer accurate because it's the date of the latest version

## save
save(study_journals, search_date, all_dois, file = 'data/0_api_search.RData')


### checks ###
## search for specific papers
# a) title
url_start = filter(study_journals, num == 1) %>% pull(api_start)
this_query = 'Epitope mapping'
url = paste(url_start, date_query, sep='')
destfile = 'api_query.xml'
download_xml(url = url, file = destfile) # run the query and download the result
# b) doi
this_doi = '10.12688/f1000research.20633.1'
url = paste('https://f1000research.com/extapi/article/xml?doi=', this_doi, sep='')
destfile = 'api_query.xml'
download_xml(url = url, file = destfile) # run the query and download the result

# look for missing earlier versions
check = filter(all_dois, str_detect(doi, '\\.v?2$')) %>% # only version 1
  mutate(doi = str_replace_all(doi, '2$', '1'),  # make into version 2
         imade = TRUE) %>%
  select(doi, imade)
check_it = left_join(check, all_dois, by = 'doi')
table(is.na(check_it$imade))

