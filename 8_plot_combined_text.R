# 8_plot_combined_text.R
# plot combined results from text analysis
# October 2025
library(ggplot2)
library(dplyr)

# from 7_text_differences_reviews_cited_other.R
load('results/7_text_other_cited.RData')
other = to_plot_plus
# from 7_text_differences_reviews_self_cited.R
load('results/7_text_self_cited.RData')
self = to_plot_plus
#
to_plot = bind_rows(self, other, .id = 'group') %>%
  select(-x) # remove previous x-axis
# make new order
new_order = group_by(to_plot, vars) %>%
  summarise(xmean = mean(coefs),
            smallp = min(pvalue)) %>%
  filter(smallp <0.05) %>% # to reduce clutter
  arrange(xmean) %>%
  ungroup() %>%
  mutate(x = 1:n())
#
to_plot_clutter = left_join(new_order, to_plot, by = 'vars') 
xlabs = select(to_plot_clutter, x, vars_plus) %>% unique()
to_plot_clutter = left_join(new_order, to_plot, by = 'vars') %>%
  mutate(x = ifelse(group==1, x-0.1, x+0.1)) # jitter by group

# text
text1 = data.frame(x = 3, y = -0.007, lower=0, upper=0, label = 'Favours\nReservations\nor Not approved') 
text2 = data.frame(x = nrow(xlabs) - 1.2, y = 0.007, lower=0, upper=0, label = 'Favours\nApproved') 
#
colours = c('darkorchid3','darkorange2')
# used minus to flip all results to Pr(Approved)
plot_plus = ggplot(data = to_plot_clutter, aes(x = x, y = -mean, ymin= -lower, ymax= -upper, col = group)) +
  geom_point(data = to_plot_clutter, aes(x=x, y= -coefs), pch=1, col='black', size=2)+
  geom_errorbar(width=0)+
  geom_point()+
  geom_hline(lty=2, yintercept=0)+
  geom_label(data = text1, aes(x=x, y=y, label=label), size=2.5, adj=1, col='grey50')+ # left-align
  geom_label(data = text2, aes(x=x, y=y, label=label), size=2.5, adj=0, col='grey50')+ # right-align
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.margin = margin(t=0, r=0, b=-3.8, l=0, "mm"), # need minus to avoid gap
        plot.margin=unit(x=c(t=-1,r=1,b=0,l=0),units="mm"), # need minus
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        legend.position = "top")+
  scale_color_manual('Included citations to:', values=colours, labels=c('Their own articles','Other articles'))+
  scale_x_continuous(breaks = 1:nrow(xlabs), labels = xlabs$vars_plus, expand=c(0.02,0.02))+
  xlab(NULL)+
  ylab('Difference in the probability of the reviewer\nrecommending Approved')+
  coord_flip()
plot_plus

# export
ggsave('figures/8_text_both.jpg', plot_plus, width = 5, height = 5.2, units = 'in', dpi = 500)

