

<<<<<<< HEAD
generate_data_binom <- function(nlim = NULL, rseed=102) {
  ev <- load_events_data(2007)
  if (!is.null(nlim)) {
    idx <- sample(1:nrow(ev), nlim)
    ev <- ev[idx,]
  }
=======
#' @import dplyr
#' @import magrittr

#' @export
get_events <- function() {

  ev <- BProDRA::load_events_data(2007)

  cc <- which( (ev$EVENT_CD <=3 ) |
                 (ev$EVENT_CD >= 20) |
                 (ev$EVENT_CD >= 14 & ev$EVENT_CD <= 16))

  cc_po <- which(ev$EVENT_CD == 2)
>>>>>>> f78300d0755813fdc27024202a172be3e0e6969d
  cc_hr <- which(ev$EVENT_CD == 23)
  cc_so <- which(ev$EVENT_CD == 3)
  cc_bb <- which( (ev$EVENT_CD >= 14) & (ev$EVENT_CD <= 16) )
  cc_h <- which( (ev$EVENT_CD >= 20) & (ev$EVENT_CD <= 22) )

  ev$outcome <- 1

  ev[cc_hr,]$outcome <- 2
  ev[cc_so,]$outcome <- 3
  ev[cc_bb,]$outcome <- 4
  ev[cc_h,]$outcome <- 5
  ev$outcome <- as.integer(ev$outcome)

  bat_ids <- unique(ev$BAT_ID)
  pit_ids <- unique(ev$PIT_ID)
  stad_ids <- unique(ev$HOME_TEAM_ID)
  ev %<>% mutate(bid=as.integer(as.factor(BAT_ID)))
  ev %<>% mutate(pid=as.integer(as.factor(PIT_ID)))
  ev %<>% mutate(sid=as.integer(as.factor(HOME_TEAM_ID)))
  ev %<>% mutate(pid = pid+max(bid))
  ev %<>% mutate(sid = sid+max(pid))
  ev %<>% select(GAME_ID, EVENT_ID, BAT_ID, PIT_ID, HOME_TEAM_ID, bid, pid, sid, outcome)

  ev
}

#' @export
generate_data_ravel <- function(ev=NULL, nlim = NULL, rseed=102) {
  if (is.null(ev)) {
    ev <- get_events()
  }

  if (!is.null(nlim)) {
    nlim <- min(nlim, nrow(ev))
    idx <- sample(1:nrow(ev), nlim)
    ev <- ev[idx,]
  }

  xx <- model.matrix(outcome ~ bid + pid + sid, data=ev)[,-1]
  max_levels <- max(xx)
  LEVELS <- c(length(bat_ids), length(pit_ids), length(stad_ids))
  sum_levels = sum(LEVELS)
  ans <- list(N=dim(xx)[[1]],
              D=dim(xx)[[2]],
              K=length(unique(ev$outcome)),
              LEVELS=LEVELS,
              x=xx,
              y=ev$outcome,
              MAX_LEVEL=max_levels,
              SUM_LEVELS=sum_levels,
              ev=ev)
}

#' @export
generate_data_vector <- function(ev=NULL, nlim = NULL, rseed=102) {

<<<<<<< HEAD
update_ans_binom <- function(ans, mod) {

  ans$theta <- matrix(rep(0, ans$D * 1), ncol=ans$D)
  thetas <- sapply(mod@theta, max, 1e-6)
  ans$theta[1,] <- thetas

  ans
}

do_fit <- function(ans, warmup=100, iter=500, init=0, seed=10101) {
  stan(file='data/multinom_ravel.stan',
=======
   if (is.null(ev)) {
    ev <- get_events()
  }

  if (!is.null(nlim)) {
    nlim <- min(nlim, nrow(ev))
    idx <- sample(1:nrow(ev), nlim)
    ev <- ev[idx,]
  }

  dmy_v <- caret::dummyVars(outcome ~ BAT_ID + PIT_ID + HOME_TEAM_ID, data=ev)
  dummy_mat <- predict(dmy_v, newdata=ev)
  jj <- cbind(as.data.frame(dummy_mat), outcome=ev$outcome)
  xx <- model.matrix(outcome ~ ., data=jj)

  ans <- list(N=dim(xx)[[1]],
              D=dim(xx)[[2]],
              K=length(unique(ev$outcome)),
              x=xx,
              y=ev$outcome,
              ev=ev)
}

#' @export
do_stan_fit_ravel <- function(ans, warmup=100, iter=500, init=0, seed=10101) {
  rstan::stan(file='inst/extdata/multinom_ravel.stan',
>>>>>>> f78300d0755813fdc27024202a172be3e0e6969d
       data=ans,
       iter=iter,
       warmup=warmup,
       init=init,
       seed=seed,
       cores=4, chains = 4)
}

#' @export
do_stan_fit_vector <- function(ans, warmup=100, iter=500, init=0, seed=10101) {
  rstan::stan(file='inst/extdata/multinom_vector.stan',
              data=ans,
              iter=iter,
              warmup=warmup,
              init=init,
              seed=seed,
              cores=4, chains = 4)
}
