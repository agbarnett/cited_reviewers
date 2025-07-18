# put together citations and exclusions
# July 2024

concatenate_citation = function(inlist){
  n_lists = length(lengths(inlist))
  excluded = citations = NULL
  for(k in 1:n_lists){
    citations = bind_rows(citations, inlist[[k]][[1]]) 
    excluded = bind_rows(excluded, inlist[[k]][[2]])
  }
  to_return = list()
  to_return$citations = citations
  to_return$excluded = excluded
  return(to_return)
}