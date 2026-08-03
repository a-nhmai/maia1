library(rstan)
library(knitr)
install.packages("blavaan")
library(blavaan)
library(lavaan)
library(MASS)
library(mvtnorm)
library(tidyverse)
install.packages("semPlot")
library(semPlot)
library(magrittr)
library(Matrix)

# setup
J <- 1000
I <- 6
K <- 2
psi <- matrix(c(1, 0.5,
                0.5, 0.8), nrow = K)  
beta <- seq(1, 2, by = .2)

# loading matrix
Lambda <- cbind(c(1, 1.5, 2, 0, 0, 0), c(0, 0, 0, 1, 1.5, 2))

# error covariance
Theta <- diag(0.3, nrow = I)

# factor scores
eta <- mvrnorm(J, mu = c(0, 0), Sigma = psi)

# error term
epsilon <- mvrnorm(J, mu = rep(0, ncol(Theta)),Sigma = Theta)

dat <- tcrossprod(eta, Lambda) + epsilon
dat_cfa  <-  dat |> as.data.frame() |> setNames(c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6"))

#Model definition
lavaan_cfa <- 'eta1 =~ Y1 + Y2 + Y3
               eta2 =~ Y4 + Y5 + Y6'

semPaths(semPlotModel_lavaanModel(lavaan_cfa))


# blavaan
blav_cfa_fit <- bcfa(lavaan_cfa, data=dat_cfa, mcmcfile = T)

summary(blav_cfa_fit)

#checking default priors
(default_prior <- dpriors())

#setting new prior
(new_prior <- dpriors(beta = "normal(0, 1)"))

new_blav <- bcfa(lavaan_cfa, data=dat_cfa, dp = new_prior)
summary(new_blav)
