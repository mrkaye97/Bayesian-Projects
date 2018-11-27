rm(list = ls())
library(rethinking)
library(MASS)
library(tidyverse)

eagles <- eagles %>%
  mutate(P = ifelse(P == "S", 1, 0),
         A = ifelse(A == "I", 1, 0),
         V = ifelse(V == "S", 1, 0))

no.interaction <- map2stan(
  alist(y ~ dbinom(n, p),
        logit(p) <- a + bp*P + ba*A + bv*V,
        a ~ dnorm(0, 10),
        bp ~ dnorm(0, 5),
        ba ~ dnorm(0, 5),
        bv ~ dnorm(0, 5)
        ), data = eagles, iter = 4000, warmup = 1000, chains = 2)

interaction <- map2stan(
  alist(y ~ dbinom(n, p),
        logit(p) <- a + bp*P + ba*A + bv*V + bi*P*A,
        a ~ dnorm(0, 10),
        bp ~ dnorm(0, 5),
        ba ~ dnorm(0, 5),
        bv ~ dnorm(0, 5),
        bi ~ dnorm(0, 5)
  ), data = eagles, iter = 4000, warmup = 1000, chains = 2)

save(interaction, no.interaction, file = "stan_fit_ps8q2.Rdata")

