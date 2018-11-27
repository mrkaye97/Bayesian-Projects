rm(list = ls())

library(rethinking)
challenger <- read.csv("https://aloy.rbind.io/data/challenger.csv") %>%
  mutate(std_temp = (temp - mean(temp)) / sd(temp))


fit.unstd <- rethinking::map2stan(flist = alist(
  oring ~ dbinom(1, p),
  logit(p) <- b0 + b1*temp,
  b0 ~ dnorm(0, 10),
  b1 ~ dnorm(0, 5)
  ), data = challenger, iter = 4000, warmup = 1000, chains = 2)

fit.std <- rethinking::map2stan(flist = alist(
  oring ~ dbinom(1, p),
  logit(p) <- b0 + b1*std_temp,
  b0 ~ dnorm(0, 10),
  b1 ~ dnorm(0, 2)
), data = challenger, iter = 4000, warmup = 1000, chains = 2)


save(fit.unstd, fit.std, file = "stan_fit_ps8q1.Rdata")

