
library(dplyr)
library(lme4)
library(BProDRA)
library(magrittr)
library(rstan)

generate_data_binom <- function(nlim = NULL, rseed=102) {
  ev <- load_events_data(2016)

  # rm pitchers as hitters
  ev %<>% filter(BAT_FLD_CD != 1)

  # anyone with less than 20 PA hitting is generic
  lowpa_batters <- ev %>% group_by(BAT_ID) %>% summarise(PA=n()) %>% filter(PA<=20) %$% BAT_ID

  # anyone with less than 20 PA hitting is generic
  lowpa_pitchers <- ev %>% group_by(PIT_ID) %>% summarise(PA=n()) %>% filter(PA<=20) %$% PIT_ID

  if (length(lowpa_batters) > 0) {
    cc <- which(ev$BAT_ID %in% lowpa_batters)
    ev[cc,]$BAT_ID <- "xxxxb001"
  }

  if (length(lowpa_pitchers) > 0) {
    cc <- which(ev$PIT_ID %in% lowpa_pitchers)
    ev[cc,]$PIT_ID <- "xxxxp001"
  }

  if (!is.null(nlim)) {
    idx <- sample(1:nrow(ev), nlim)
    ev <- ev[idx,]
  }


  cc_hr <- which(ev$EVENT_CD == 23)
  cc_so <- which(ev$EVENT_CD == 3)
  cc_bb <- which( (ev$EVENT_CD >= 14) & (ev$EVENT_CD <= 16) )
  cc_h <- which( (ev$EVENT_CD >= 20) & (ev$EVENT_CD <= 22) )

  ev$outcome <- 0
  ev[cc_hr,]$outcome <- 1

  ev$outcome <- as.integer(ev$outcome)

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
    ev <- generate_data_binom(nlim=nlim, rseed=rseed)
  }

  if (!is.null(nlim)) {
    nlim <- min(nlim, nrow(ev))
    idx <- sample(1:nrow(ev), nlim)
    ev <- ev[idx,]
  }


  bat_ids <- unique(ev$BAT_ID)
  pit_ids <- unique(ev$PIT_ID)
  stad_ids <- unique(ev$HOME_TEAM_ID)
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


update_ans_binom <- function(ans, mod=NULL) {

  if (is.null(mod)) {
    mod <- glmer(outcome ~ (1|bid) + (1|pid) + (1|sid),
                 data=ans$ev, family=binomial(),
                 nAGQ = 0, control=glmerControl(optimizer = "nloptwrap"))
  }

  ans$RANEF_SIGMA <- matrix(rep(0, ans$D * 1), ncol=ans$D)
  thetas <- sapply(mod@theta, max, 1e-6)
  ans$RANEF_SIGMA[1,] <- thetas

  ans$RANEF_SIGMA <- as.numeric(ans$RANEF_SIGMA)


  rr <- rbind(ranef(mod)$bid,ranef(mod)$pid,ranef(mod)$sid)
  ans$rr <- rr
  ans
}

get_init_fun <- function(ans) {
  rr <- ans$rr
  function(idx=NULL) {
    rr
  }
}

do_fit <- function(ans, warmup=100, iter=500, seed=10101) {
  init_fun <- get_init_fun(ans)
  stan(file='inst/extdata/binom_ravel_fixtheta.stan',
       data=ans,
       iter=iter,
       warmup=warmup,
       init=init_fun,
       seed=seed,
       cores=4, chains = 4)
}

