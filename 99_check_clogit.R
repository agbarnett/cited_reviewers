# 99_check_clogit.R
# sanity check that conditional logistic regression can be done using coxph
# June 2025
library(survival)
library(dplyr)

# using clogit example from survival library
resp <- levels(logan$occupation)
n <- nrow(logan)
indx <- rep(1:n, length(resp))
logan2 <- data.frame(logan[indx,],
                     id = indx,
                     tocc = factor(rep(resp, each=n)))
logan2$case <- (logan2$occupation == logan2$tocc)
cmodel = clogit(case ~ tocc + tocc:education + strata(id), logan2)
summary(cmodel)

# now using coxph
logan2 = mutate(logan2, time = 1) # same time for everyone
frailty = coxph(Surv(time, event = case) ~ tocc + tocc:education + strata(id), logan2, ties='exact')
summary(frailty)