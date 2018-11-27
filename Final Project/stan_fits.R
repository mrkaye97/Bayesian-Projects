library(rethinking)
library(data.table)
library(tidyverse)

df <- fread("data.csv") %>%
  data.frame() %>%
  mutate(TB = X1B + 2*X2B + 3*X3B + 4*HR_h) %>%
  select(1:2, 31, 10:14, 19:23, 25:30, 32)

df_scaled <- df %>%
  mutate_at(vars(4:20), funs(scale(.)))

war.mod <- map2stan(flist = alist(
  Wins ~ dpois(l),
  log(l) <- a + b*WAR,
  a ~ dnorm(4, 2),
  b ~ dnorm(0, .5)
), data = df_scaled, iter = 10000, warmup = 1000, chains = 6, cores = 6, WAIC = FALSE)

otherstats.mod <- map2stan(flist = alist(
  Wins ~ dpois(l),
  log(l) <- a + b_ER * ER + b_BB_p * BB_p + b_K_p * K_p + b_TB * TB + b_R * R + b_SB * SB + b_BB_h * BB_h + b_K_h * K_h, 
  a ~ dnorm(4, 3),
  c(b_ER, b_BB_p, b_K_p, b_TB, b_R, b_SB, b_BB_h, b_K_h) ~ dnorm(0,.1)
), data = df_scaled, iter = 10000, warmup = 4000, chains = 6, cores = 6, WAIC = FALSE)

save(war.mod, otherstats.mod, file = "stan_fits_update.Rdata")
