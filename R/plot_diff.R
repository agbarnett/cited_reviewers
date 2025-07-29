# plot_diff.R
# plot the difference in estimates between those using the full data and those using main reviewers only
# called by 6_models.R
# June 2025

plot_diff = function(co_reviewers, referees_only){
  
  # concatenate
  to_plot = bind_rows(co_reviewers, referees_only, .id = 'id') %>%
    filter(confounder == '') %>% # for main results
    mutate(Version = ifelse(version==1, "Version 1", 'Version 2+'), # make nicer facet labels
           Outcome = ifelse(outcome==1, 'Approved vs\nReservations/Not approved', 'Approved/Reservations vs\nNot approved')) 
  
  #
  colours = c('darkred','darkseagreen')
  gplot = ggplot(data = to_plot, aes(x = id, y = coef, ymin=lower, ymax = upper, col=factor(id))) +
    geom_hline(yintercept=1, lty=2, col='grey33')+
    geom_point(size=5)+
    geom_errorbar(width=0, linewidth=1.1)+
    xlab('')+
    ylab('Odds ratio')+
    scale_color_manual('Co-reviewers', values = colours, labels=c('Yes','No'))+
    scale_x_discrete(breaks=NULL) + # blank axis
    scale_y_log10()+
    theme_bw()+
    theme(axis.ticks.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "inside", 
          legend.position.inside =  c(0.85, 0.15))+
    coord_flip()+
    facet_wrap(~facet + Outcome + Version, scales='free')
  
  return(gplot)
  
} # end of function
