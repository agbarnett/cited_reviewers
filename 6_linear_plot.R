# 6_linear_plot.R
# make figures for linear associations
# June 2025
library(gridExtra)

l1 = 'Approve vs\nReservations/Reject' # short labels
l2 = 'Approve/Reservations\nvs Reject'

# get the right data
for_plot1 = filter(results1_only_referees$results, clabel == 'log')
for_plot2 = filter(results2_only_referees$results, clabel == 'log') # plot results for log-transformed works count as that was generally the best fit, see AIC below; although does not really matter, see eplot
for_plot = bind_rows(for_plot1, for_plot2, .id = 'rq') %>%
  filter(type== 'linear') %>%
  mutate(facet = ifelse(version==1, 'Version = 1', 'Versions = 2+'))

# extrapolate linear effects
to_plot = NULL
for (k in 1:nrow(for_plot)){
  for (citations in 1:4){
    this_plot = for_plot[k,] %>%
      mutate(citations = citations,
             or = exp(log(coef)*citations)) # 
  to_plot = bind_rows(to_plot, this_plot)
  }
}

# make the plot
vcolours = c('darkseagreen3','dodgerblue')
# text
text1 = data.frame(model = 1.5, y = 1, lower = NA, upper = NA, version=1, outcome=1, rq = 1, label = 'More favourable\nrecommendation') 
text2 = data.frame(model = 1.5, y = 1, lower = NA, upper = NA, version=1, outcome=1, rq = 1, label = 'Less favourable\nrecommendation') 
# odds ratio label with arrows
ylab = "Odds ratio\n(Approve \u2192 Reservations \u2192 Reject)" # using symbol for the right arrow

# break into two plots

#
plotl1 = ggplot(data = filter(to_plot, rq == 1), aes(x = citations, y = or, col = factor(outcome)))+
  ggtitle('Question 1: reviewer cited')+
  geom_hline(yintercept=1, lty=2, col='grey33')+
  geom_line()+
  geom_point(size=3)+
  xlab(NULL) + # done in plot below
  ylab(' \n ') + # done in plot below
  scale_color_manual('Recommendation', values = vcolours, labels=c(l1, l2))+
  scale_y_log10(breaks = c(0.7,1,1.5)) + #breaks = breaks_fun)+ # see above
  theme_bw()+
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none', # show legend in next plot
        legend.key.width = unit(36, 'pt'), # make key wider to show CIs
        legend.key.spacing.y = unit(8, "pt"))+ # increase space in legend because of long text
  facet_wrap(~facet)

#
plotl2 = ggplot(data = filter(to_plot, rq == 2), aes(x = citations, y = or, col = factor(outcome)))+
  ggtitle('Question 2: reviewer asks for self-citation')+
  geom_hline(yintercept=1, lty=2, col='grey33')+
  geom_line()+
  geom_point(size=3)+
  xlab('Number of citations')+
  ylab(ylab)+
  scale_color_manual('Recommendation', values = vcolours, labels=c(l1, l2))+
  scale_y_log10() + #breaks = breaks_fun)+ # see above
  theme_bw()+
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(36, 'pt'), # make key wider to show CIs
        legend.key.spacing.y = unit(8, "pt"))+ # increase space in legend because of long text
  facet_wrap(~facet)

# export figures
jpeg(filename = 'figures/6_linear_predictor.jpg',  
     width = 5.5, height = 5.5, units = 'in', res = 500)
grid.arrange(plotl1, plotl2, ncol = 1, heights = c(1,1.3)) # heights by trial and error
dev.off()

