
library(rstan)

set.seed(101)
npt <- 1000
xx <- matrix(rnorm(npt * 3), ncol=3)

beta0 <- matrix(c(2, 5, 7), ncol=1)
noise = rnorm(npt, 0, 2)

y_lin <- xx %*% beta0
e_truth <- exp(y_lin)/(1+exp(y_lin))
y_exp <- exp(y_lin+noise)/(1 + exp(y_lin+noise))
y <- as.integer(y_exp >= 0.5)


do_fit <- function() {
  stan(file='data/logreg.stan',
       data=list(N=npt, D=3, x=xx, y=y))
}
