library(tidyverse)


N <- 1000


set.seed(2023)

data_surv <- tibble(tt = 0, d = NA, exposed = N) %>% 
  bind_rows(tibble(tt = rgeom(N, prob = .3) %>% cumsum()) %>% 
              mutate(tt = tt + 1) %>% 
              group_by(tt) %>% 
              summarise(d = n()) %>% 
              ungroup() %>% 
              mutate(exposed = N - cumsum(d))) %>% 
  mutate(S = exposed / N) %>% 
  slice(1:1000)


data_surv %>% ggplot(aes(x = tt, y = S)) + geom_point() + geom_line()


plot(seq(0, 15, by = 1), 1-pgeom(seq(0, 15, by = 1), prob = 0.3), 
     type = "s", xlab = "n", ylab = "S")


plot(seq(0, 15, by = 1), 
     -c(NA, diff((1-pgeom(seq(0, 15, by = 1), prob = 0.3)))) / (1-pgeom(seq(0, 15, by = 1), prob = 0.3)), 
     type = "s", xlab = "n", ylab = "S")

