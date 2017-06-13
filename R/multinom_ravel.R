
library(BProDRA)
library(rstan)
library(dplyr)
library(magrittr)

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

  bat_ids <- unique(ev$BAT_ID)
  pit_ids <- unique(ev$PIT_ID)
  stad_ids <- unique(ev$HOME_TEAM_ID)
  ev %<>% mutate(bid=as.integer(as.factor(BAT_ID)))
  ev %<>% mutate(pid=as.integer(as.factor(PIT_ID)))
  ev %<>% mutate(sid=as.integer(as.factor(HOME_TEAM_ID)))
  ev %<>% mutate(pid = pid+max(bid))
  ev %<>% mutate(sid = sid+max(pid))
  ev %<>% select(GAME_ID, EVENT_ID, BAT_ID, PIT_ID, HOME_TEAM_ID, bid, pid, sid, outcome)

  xx <- model.matrix(outcome ~ bid + pid + sid, data=ev)[,-1]
  max_levels <- max(xx)
  LEVELS <- c(length(bat_ids), length(pit_ids), length(stad_ids))
  sum_levels = sum(LEVELS)
  ans <- list(N=dim(xx)[[1]],
              D=dim(xx)[[2]],
              LEVELS=LEVELS,
              x=xx,
              y=ev$outcome,
              MAX_LEVEL=max_levels,
              SUM_LEVELS=sum_levels,
              ev=ev)
}


do_fit <- function(ans, warmup=100, iter=500, init=0, seed=10101) {
  stan(file='data/multinom_ravel.stan',
       data=ans,
       iter=iter,
       warmup=warmup,
       init=init,
       seed=seed,
       cores=4)
}
