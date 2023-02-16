library(tidyverse)

## Deben ser:
##             - p pequeño
##             - N grande
##             - p x N < 7 (aprox)
N <- 3600 # Número  de éxitos
P <- 0.001

## Simulamos los tiempos en que se producen los éxitos

set.seed(2023)
probe <- tibble(r_geom = rgeom(N, P),
                # Periodos en que se producen los éxitos
                r_geom_acum = cumsum(r_geom)) 

probe <- probe %>% 
  full_join(tibble(idx = 1:max(probe$r_geom_acum)),
            by = c("r_geom_acum" = "idx")) %>% 
  arrange(r_geom_acum) %>% 
    mutate(succes = ifelse(is.na(r_geom), 0, 1)) 

sum(probe$succes) # Número de éxitos
range(probe$r_geom_acum) # Número de periodos

## Vemos que el número de éxitos producidos sigue una distribución binomial

# Éxitos en ese número de periodos según una binomial
rbinom(1000, max(probe$r_geom_acum), P) %>% mean()

##  La binomial se puede aproximar con una Poisson
## Éxitos en ese periodo de tiempo según una Poisson

# Tasa en todo el periodo
lambda_periodo <- max(probe$r_geom_acum)*P
lambda_periodo

# Tasa por unidad de tiempo
lambda_u_de_t <- P
lambda_u_de_t

rpois(1000, lambda = lambda_periodo) %>% mean()

rpois(1000, lambda = lambda_u_de_t * max(probe$r_geom_acum)) %>% mean()

rpois(1000, lambda = N) %>% mean()

## La geométrica se puede aproximar por una exponencial
## Tiempos entre éxitos según una exponencial

rexp(N, rate = lambda_u_de_t) %>% cumsum() %>% max()
