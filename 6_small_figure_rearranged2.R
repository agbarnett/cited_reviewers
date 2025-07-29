# 6_small_figure_rearranged2.R
# remake small version of the figure outside of function
# re-arranged to group together results by the outcome categories
# July 2025

## get the right data
# estimates
for_plot = filter(results2_only_referees$results, 
                  clabel == 'log', # plot results for log-transformed works count as that was generally the best fit, see AIC below; although does not really matter, see eplot
                  type!= 'linear')
# predictions
for_plot_pred = filter(results2_only_referees$predictions, 
                  clabel == 'log', # plot results for log-transformed works count
                  type!= 'linear')

# make the plot
vcolours = c('darkseagreen3','dodgerblue')
vlabels = c('1','2+')
# text
text1 = data.frame(model = 1.5, y = 1.05, lower = NA, upper = NA, version=1, outcome=1, label = 'Reviewer\nmore\nfavourable') 
text2 = data.frame(model = 1.5, y = 0.95, lower = NA, upper = NA, version=1, outcome=1, label = 'Reviewer\nless\nfavourable') 
# titles to split later plots
dtitle1 = data.frame(x=0, y=0, text = "Approved versus Reservations or Not approved")
title1 = ggplot(data = dtitle1, aes(x=x, y=y, label=text))+
  geom_text(fontface = "bold")+
  xlab(NULL)+
  ylab(NULL)+
  theme_void()+
  theme(plot.margin = margin(0, 0, 0, 0, "mm"))
dtitle2 = data.frame(x=0, y=0, text = "Approved or Reservations versus Not approved")
title2 = ggplot(data = dtitle2, aes(x=x, y=y, label=text))+
  geom_text(fontface = "bold")+
  xlab(NULL)+
  ylab(NULL)+
  theme_void()+
  theme(plot.margin = margin(0, 0, 0, 0, "mm"))

#
plot_top_left = ggplot(data = filter(for_plot, outcome == 1), 
               aes(x = version, y = coef, ymin=lower, ymax = upper, col=factor(version)))+
  geom_hline(yintercept=1, lty=2, col='grey33')+
  geom_point(size=5)+
  geom_errorbar(width=0, linewidth=1.1)+
  geom_label(data = text1, aes(x=model, y=y, label=label), size=3, adj=0, col='grey66')+
  geom_label(data = text2, aes(x=model, y=y, label=label), size=3, adj=1, col='grey66')+
  xlab('Article version')+
  ylab('Odds ratio, Approved vs Reservations/Not approved')+
  scale_color_manual('Article version', values = vcolours, labels=c('1','2+'))+
  scale_x_reverse(breaks=1:2, labels=vlabels, expand=c(0.05,0.05))+ # 
  scale_y_log10(breaks=c(0.1,0.25,0.5,1), labels=c('0.1','0.25','0.5','1')) + 
  theme_bw()+
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=9),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.key.width = unit(36, 'pt'), # make key wider to show CIs
        legend.key.spacing.y = unit(8, "pt"))+ # increase space in legend because of long text
  coord_flip() 
plot_top_left

#
plot_bottom_left = ggplot(data = filter(for_plot, outcome == 2), 
                       aes(x = version, y = coef, ymin=lower, ymax = upper, col=factor(version)))+
  geom_hline(yintercept=1, lty=2, col='grey33')+
  geom_point(size=5)+
  geom_errorbar(width=0, linewidth=1.1)+
  geom_label(data = text1, aes(x=model, y=y, label=label), size=3, adj=0, col='grey66')+
  geom_label(data = text2, aes(x=model, y=y, label=label), size=3, adj=1, col='grey66')+
  xlab('Article version')+
  ylab('Odds ratio, Approved/Reservations vs Not approved')+
  scale_color_manual('Article version', values = vcolours, labels=c('1','2+'))+
  scale_x_reverse(breaks=1:2, labels=vlabels, expand=c(0.05,0.05))+ # 
  scale_y_log10(breaks=c(0.1,0.25,0.5,1), labels=c('0.1','0.25','0.5','1')) + 
  theme_bw()+
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=9),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.key.width = unit(36, 'pt'), # make key wider to show CIs
        legend.key.spacing.y = unit(8, "pt"))+ # increase space in legend because of long text
  coord_flip() 
plot_bottom_left


## probability plots
labels = c('No self-\ncitation\n(version 1)','Self-\ncitation\n(version 1)',
           'No self-\ncitation\n(version 2+)','Self-\ncitation\n(version 2+)')
plot_top_right = ggplot(data = filter(for_plot_pred, outcome==1), 
               aes(x = version + (pred/2), y = prob, ymin=lower, ymax = upper, col=factor(version)))+
  geom_point(size=5)+
  geom_errorbar(width=0, linewidth=1.1)+
  xlab('')+
  ylab('Probability of Approved')+
  scale_color_manual('Article version:', values = vcolours, labels=c('1','2+'))+
  scale_x_reverse(breaks=seq(1,2.5,0.5), labels=labels)+ # reverse version 1 at top
  theme_bw()+
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1.5, 'cm'), # bigger lines in legend 
        legend.margin = margin(0, 0, 0, 0, "cm"), 
        legend.box.margin = margin(0, 0, 0, 0, "cm"), 
        legend.title = element_text(hjust = 0.9), # move to right
        legend.position = 'top')+
  coord_flip() # 
plot_top_right
# get legend
glegend = g_legend(plot_top_right) # get legend ... 
plot_top_right = plot_top_right + theme(legend.position = 'none')  # ... now turn off legend
#
plot_bottom_right = ggplot(data = filter(for_plot_pred, outcome==2), 
                        aes(x = version + (pred/2), y = prob, ymin=lower, ymax = upper, col=factor(version)))+
  geom_point(size=5)+
  geom_errorbar(width=0, linewidth=1.1)+
  xlab('')+
  ylab('Probability of Approved/Reservations')+
  scale_color_manual('Article version', values = vcolours, labels=c('1','2+'))+
  scale_x_reverse(breaks=seq(1,2.5,0.5), labels=labels)+ # reverse version 1 at top
  theme_bw()+
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')+
  coord_flip() # 
plot_bottom_right


# put legend as number 1 to avoid overlap:
lmatrix = matrix(c(NA,1,1,NA,
                   2,2,2,2,
                   3,3,4,4,
                   5,5,5,5,
                   6,6,7,7), ncol=4, byrow=TRUE)
#
jpeg('figures/6_self_cited_withp.jpg', width = 6.8, height = 6.5, units = 'in', res = 500)
grid.arrange(glegend, # odd order, but done to avoid overlap
             title1,
             plot_top_left, plot_top_right, 
             title2,
             plot_bottom_left, plot_bottom_right, 
             widths = c(5,1,1,5),
             heights=c(0.1, 0.1, 1, 0.1, 1), layout_matrix = lmatrix)
dev.off()
