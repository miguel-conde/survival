library(tidyverse)


N <- 100
p <- 0.1


set.seed(2023)

data_surv <- tibble(tt = 0, d = NA, exposed = N) %>% 
  bind_rows(tibble(tt = rgeom(N, prob = p)) %>%
              arrange(tt) %>% 
              mutate(tt = tt + 1) %>% 
              group_by(tt) %>% 
              summarise(d = n()) %>% 
              mutate(exposed = N - cumsum(d))) %>% 
  mutate(S = exposed / N) %>% 
  full_join(tibble(tt = 1:N, S_th = 1 - pgeom((1:N)-1, prob = p))) %>% 
  arrange(tt) %>% 
  slice(1:50) %>% 
  drop_na()


data_surv %>% ggplot(aes(x = tt, y = S)) + geom_point() + geom_step() +
  geom_step(aes(y = S_th), color = "red")


plot(seq(0, 50, by = 1), 1-pgeom(seq(0, 50, by = 1), prob = 0.1), 
     type = "s", xlab = "n", ylab = "S")


plot(seq(0, 15, by = 1), 
     -c(NA, diff((1-pgeom(seq(0, 15, by = 1), prob = 0.3)))) / (1-pgeom(seq(0, 15, by = 1), prob = 0.3)), 
     type = "s", xlab = "n", ylab = "S")

