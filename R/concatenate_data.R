# put together reviewers papers and exclusions
# July 2024

concatenate_data = function(inlist, position){
  n_lists = length(lengths(inlist))
  concatenated = NULL
  for(k in 1:n_lists){
    concatenated = bind_rows(concatenated, inlist[[k]][[position]]) 
  }
  return(concatenated)
}
