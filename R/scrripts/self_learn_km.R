# Survival Analysis - A self-learn text

library(tidyverse)
library(survival)

library(survminer)
library(survMisc)



leukemia_dataset <- readRDS("data/remission.Rds")

leukemia_dataset %>% 
  group_by(g) %>% 
  summarise(n_e = sum(e == 1), n_censored = sum(e == 0), n = n())

leukemia_dataset %>% 
  group_by(g) %>% 
  summarise(TT = mean(t))

leukemia_dataset %>% 
  group_by(g) %>% 
  summarise(h = sum(e) / sum(t)) %>% 
  spread(g, h) %>% 
  mutate( h1_h2 = `2` / `1`)

surv_object <- Surv(time = leukemia_dataset$t, event = leukemia_dataset$e)  

km_fit <- survfit(surv_object ~ g)
km_fit

km_fit %>% summary()

plot(km_fit, col = 1:2)
legend("topright", lty = 1, legend = c("Grupo 1", "Grupo 2"), col = 1:2, bty = "n")

ggsurvplot(km_fit, data = surv_object, surv.median.line = "hv", risk.table = TRUE)

## The log-rank test for 2 groups

# ... how to evaluate whether or
# not KM curves for two or more groups are
# statistically equivalent.
# When we state that two KM curves are “statistically
# equivalent,” we mean that, based on a testing
# procedure that compares the two curves in
# some “overall sense,” we do not have evidence to
# indicate that the true (population) survival
# curves are different.

# The null hypothesis being tested is that there is
# no overall difference between the two survival
# curves. Under this null hypothesis, the log–-
#   rank statistic is approximately chi-square with
# one degree of freedom. Thus, a P-value for the
# log–rank test is determined from tables of the
# chi-square distribution.

survdiff(surv_object ~ g)
# The log rank statistic is highly significant with a p-value p= 4e-05
# The log–rank statistic is 16.79
# and the corresponding P-value is zero to three
# decimal places. This P-value indicates that the
# null hypothesis should be rejected. We can
# therefore conclude that the treatment and placebo
# groups have significantly different KM
# survival curves.

## The log-rank test for several groups

# H0: All survival curves are the same.

## Alternatives to the log-rank test

# There are several alternatives to the log rank test : the Wilcoxon, 
# the Tarone-Ware, the Peto, and the Flemington-Harrington test.

# Stratified log-rank test

## Confidence intervals for KM curves

## Confidence intervals for the median survival times


