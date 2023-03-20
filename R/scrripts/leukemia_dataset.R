# Survival Analysis - A self-learn text

library(tidyverse)
library(survival)

library(survminer)
library(survMisc)

t1 <- c(6, 6, 6, 7, 10,
        13, 16, 22, 23,
        6, 9, 10, 11,
        17, 19, 20,
        25, 32, 32,
        34, 35)

t2 <- c(1, 1, 2, 2, 3,
        4, 4, 5, 5,
        8, 8, 8, 8,
        11, 11, 12, 12,
        15, 17, 22, 23)

e1 <- c(1, 1, 1, 1, 1,
        1, 1, 1, 1,
        0, 0, 0, 0,
        0, 0, 0,
        0, 0, 0,
        0, 0)

e2 <- rep(1, length(t2))

log_wbc_1 <- c(2.31, 
               4.06, 
               3.28, 
               4.43, 
               2.96, 
               2.88, 
               3.60, 
               2.32, 
               2.57, 
               3.20, 
               2.80, 
               2.70, 
               2.60, 
               2.16, 
               2.05, 
               2.01, 
               1.78, 
               2.20, 
               2.53, 
               1.47, 
               1.45)

log_wbc_2 <- c(2.80,
               5.00,
               4.91,
               4.48,
               4.01,
               4.36,
               2.42,
               3.49,
               3.97,
               3.52,
               3.05,
               2.32,
               3.26,
               3.49,
               2.12,
               1.50,
               3.06,
               2.30,
               2.95,
               2.73,
               1.97)

g <- c(rep(1, length(t1)), rep(2, length(t2)))

leukemia_dataset <- tibble(t = c(t1, t2), e = c(e1, e2), 
                           log_wbc = c(log_wbc_1, log_wbc_2), g = g)

saveRDS(leukemia_dataset, "data/remission.Rds")




