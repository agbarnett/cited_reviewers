# 7_model_checks.R
# run model checks
# September 2025
source('R/clogit_model.R') # runs the conditional logistic regression models
library(survival) # for conditional logistic regression

# get the data from 5_match_papers_reviewers.R
load('data/5_analysis_data.RData')
# set up predictor
matched = mutate(matched,
                pred = as.numeric(matches > 0))

# re-run selected models
o = " != 'reject'"
f = formula = paste('I(recommendation', o , ') ~ pred + strata(doi)', sep='') # stratify by article and version
model = run_one(indata = matched,
        formula = f,
        this_v = 1,
        this_c = '', # no confounders
        this_o = o,
        this_co = FALSE,
        this_type = 'any_vs_none',
        alpha = 0.05)$model

res = residuals(model)

# not working ... need to find options
# to here

# influential diagnostics
influential = influence.measures(model)

#which(apply(influential$is.inf, 1, any))
#plot(rstudent(small_model) ~ hatvalues(small_model)) # recommended by some

## large Cook's distance
index = which(colnames(influential$infmat) == 'cook.d')
cookd = influential$infmat[,index]
index = which(cookd > 6e-04)
index_y = colnames(x) %in% x_selected_names
x_influential = x[index,index_y]
y_influential = y[index]
d_influential = cbind(y_influential, x_influential)
# plot
to_plot = data.frame(cookd)
cplot = ggplot(data = to_plot, aes(x=cookd))+
  geom_histogram(fill = cbbPalette[3])+
  xlab('Cook`s distance')+
  ylab('Count')+
  g.theme
cplot
ggsave('figures/7_cooks_distance.jpg', cplot, width = 4.2, height=4.2, units='in', dpi=500)
cat('The largest Cook`s distance was ', format(max(to_plot$cookd), scientific=FALSE), '.\n', sep='')

## largest df-betas
dfb = dfbeta(small_model)
to_plot = data.frame(dfb) %>%
  reshape::melt() %>%
  mutate(variable = str_remove(variable, '^X.|x_selected'))
dplot = ggplot(to_plot, aes(x=value))+
  geom_histogram()+
  facet_wrap(~variable, scales='free')+
  g.theme
dplot
#
mutate(to_plot, abs = abs(value)) %>%
  arrange(value) %>%
  head()

# check colinearity, car does not work as it expects model.matrix
vif = vif_matrix(small_model)
to_export = data.frame(vif) %>%
  tibble::rownames_to_column() %>%
  mutate(# labels
    rowname = str_replace(rowname, '^type', 'type_'), # type did not start with _
    rowname = str_replace(rowname, '_', ' = '), # replace first underbar with equals ...
    rowname = str_replace_all(rowname, '_', ' ') # ... and then remaining with space
  )
# export to latex
print(xtable(to_export, digits=2), include.rownames=FALSE, hline.after=FALSE, file = "results/3_vif.tex")

