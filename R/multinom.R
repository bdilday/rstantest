
library(BProDRA)
library(rstan)
library(dplyr)
library(magrittr)

set.seed(101)
npt <- 1000
xx <- matrix(rnorm(npt * 3), ncol=3)

beta0 <- matrix(c(2, 5, 7), ncol=1)
noise = rnorm(npt, 0, 2)

y_lin <- xx %*% beta0
e_truth <- exp(y_lin)/(1+exp(y_lin))
y_exp <- exp(y_lin+noise)/(1 + exp(y_lin+noise))
y <- as.integer(y_exp >= 0.5)


generate_data <- function(nlim = NULL, rseed=102) {
  ev <- load_events_data(2007)
  if (!is.null(nlim)) {
    idx <- sample(1:nrow(ev), nlim)
    ev <- ev[idx,]
  }
  cc_hr <- which(ev$EVENT_CD == 23)
  cc_so <- which(ev$EVENT_CD == 3)
  ev$outcome <- 0

  ev[cc_hr,]$outcome <- 1
  ev[cc_so,]$outcome <- 0

  bat_id <- unique(ev$BAT_ID)
  pit_ids <- unique(ev$PIT_ID)
  stad_ids <- unique(ev$HOME_TEAM_ID)
  ev %<>% mutate(bid=as.integer(as.factor(BAT_ID)))
  ev %<>% mutate(pid=as.integer(as.factor(PIT_ID)))
  ev %<>% mutate(sid=as.integer(as.factor(HOME_TEAM_ID)))
  ev %<>% select(GAME_ID, EVENT_ID, BAT_ID, PIT_ID, HOME_TEAM_ID, bid, pid, sid, outcome)

  xx <- model.matrix(outcome ~ bid + pid + sid, data=ev)
#  xx <- model.matrix(outcome ~ sid - 1, data=ev)
  max_levels <- max(xx)

  ans <- list(N=dim(xx)[[1]], D=dim(xx)[[2]], x=xx, y=ev$outcome, MAX_LEVEL=max_levels, ev=ev)
}



gg <- function(x) {
  cc1 <- which(tt$outcome == 1)
  cc0 <- which(tt$outcome == 0)
  
  lam <- x[tt$bid] + x[tt$pid] + x[tt$sid] + x[1029]
  ee <- exp(lam)
  sum(tt$outcome * lam - log(1+ee)) - 
    sum(x[1:514]**2) -
    sum(x[515:998]**2) - 
    sum(x[999:1028]**2)
  
}

do_fit <- function(ans) {
  stan(file='data/multinom.stan', data=ans, iter=500, warmup=100, init=0, seed=10101, cores=4)
}
