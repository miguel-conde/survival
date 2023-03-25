library(tidyverse)


# Proceso estocástico Poisson / Exponencial -------------------------------

LAMBDA <- 1/2 # Eventos por u. de t.
N <- 10000

set.seed(2023)

r_data <- tibble(
  # Duración media = 1/lambda
  durations = rexp(N, rate = LAMBDA),
  event_time = cumsum(durations)
) %>% 
  mutate(intervals = cut(event_time, 
                         seq(0, floor(max(event_time))+1,
                             by = 1),
                         right = FALSE))

# Los instantes de los eventos se distribuyen uniformemente
r_data$event_time %>% summary()
r_data %>% 
  ggplot(aes(x = event_time)) +
  geom_density()

# Los intervalos entre eventos hemos hecho que vengan
# de una exponencial
r_data$durations %>% summary()
r_data %>% 
  ggplot(aes(x = durations)) +
  geom_density()

# Así qu el número de eventos por u. de t.
# debería ser una poisson de parámetro LAMBDA,
# o sea, media LAMBDA
n_events <- r_data %>% 
  group_by(intervals) %>% 
  summarise(n_events = n()) %>% 
  full_join(tibble(intervals = levels(r_data$intervals))) %>% 
  arrange(intervals) %>% 
  mutate(n_events = ifelse(is.na(n_events), 0, n_events))

n_events$n_events %>% summary()
n_events %>% 
  ggplot(aes(x = n_events)) +
  geom_histogram(bins = 20)


# El evento i tiene lugar en un instante con distribución
# gamma(shape = i, rate = LAMBDA), cuya media es 
# shape / rate = i/LAMBDA
plot((1:N)/LAMBDA, r_data$event_time)
abline(a=0,b=1, col = "red")
cor((1:N)/LAMBDA, r_data$event_time)


# Curva survival de una geométrica ----------------------------------------


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
  full_join(tibble(tt = 1:N, S_th_exp = 1 -  pexp((1:N)-1, rate = p*N/N))) %>% 
  arrange(tt) %>% 
  slice(1:50) %>% 
  drop_na(expected)


data_surv %>% ggplot(aes(x = tt, y = S)) + geom_point() + geom_step() +
  geom_step(aes(y = S_th), color = "red") +
  geom_step(aes(y = S_th_exp), color = "blue")


plot(seq(0, 50, by = 1), 1-pgeom(seq(0, 50, by = 1), prob = 0.1), 
     type = "s", xlab = "n", ylab = "S")


plot(seq(0, 15, by = 1), 
     -c(NA, diff((1-pgeom(seq(0, 15, by = 1), prob = 0.3)))) / (1-pgeom(seq(0, 15, by = 1), prob = 0.3)), 
     type = "s", xlab = "n", ylab = "S")

