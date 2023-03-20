# Survival Analysis - A self-learn text

library(tidyverse)
library(survival)

library(survminer)
library(survMisc)

leukemia_dataset <- readRDS("data/remission.Rds")

leukemia_dataset

remission <- leukemia_dataset %>% 
  mutate(status = ifelse(g == 1, 0, 1)) # Para que el hazard de placebo sea mayor que el de tratamiento

# ...the basic question of interest
# concerns comparing the survival experience of
# the two groups adjusting for the possible confounding
# and/or interaction effects of logWBC  

cx1 <- coxph(Surv(t, e) ~ status,           data = remission)
cx2 <- coxph(Surv(t, e) ~ status + log_wbc, data = remission)
cx3 <- coxph(Surv(t, e) ~ status*log_wbc,   data = remission)

new_data <- remission %>% group_by(status) %>% summarise(log_wbc = mean(log_wbc))
new_data <- tibble(g = 0:1, log_wbc = mean(remission$log_wbc))
surv_cx2_0 <- survfit(cx2, newdata = tibble(status = 0, log_wbc = 0))
surv_cx2 <- survfit(cx2, newdata = new_data)

plot(surv_cx2)
ggsurvplot(surv_cx2, data = new_data, conf.int = FALSE)

cx2_summary_0 <- surv_summary(surv_cx2)

ggsurvplot(cx2_summary_0)
