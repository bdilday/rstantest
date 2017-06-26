
library(BProDRA)
library(rstan)
library(dplyr)
library(magrittr)
library(lme4)

generate_event_data <- function(nlim = NULL, rseed=102) {
  ev <- load_events_data(2016)
  if (!is.null(nlim)) {
    idx <- sample(1:nrow(ev), nlim)
    ev <- ev[idx,]
  }

  cc_hr <- which(ev$EVENT_CD == 23)
  cc_so <- which(ev$EVENT_CD == 3)
  cc_bip0 <- which(ev$EVENT_CD == 2)
  cc_bip1 <- which(ev$EVENT_CD >= 20 & ev$EVENT_CD <= 22)
  cc_bb <- which(ev$EVENT_CD >= 14 & ev$EVENT_CD <= 16)

  ev$outcome <- NA

  ev[cc_bip0,]$outcome <- 0
  ev[cc_bip1,]$outcome <- 1
  ev[cc_hr,]$outcome <- 2
  ev[cc_so,]$outcome <- 3
  ev[cc_bb,]$outcome <- 4

  assertthat::are_equal(sum(is.na(ev$outcome)), 0)
  ev %<>% mutate(bid=as.integer(as.factor(BAT_ID)))
  ev %<>% mutate(pid=as.integer(as.factor(PIT_ID)))
  ev %<>% mutate(sid=as.integer(as.factor(HOME_TEAM_ID)))
  ev %<>% mutate(pid = pid+max(bid))
  ev %<>% mutate(sid = sid+max(pid))
  ev %<>% select(GAME_ID, EVENT_ID, BAT_ID, PIT_ID, HOME_TEAM_ID, bid, pid, sid, outcome)

  ev
}

generate_model_df <- function(event_data) {
  bat_ids <- unique(ev$BAT_ID)
  pit_ids <- unique(ev$PIT_ID)
  stad_ids <- unique(ev$HOME_TEAM_ID)

  xx <- model.matrix(outcome ~ bid + pid + sid, data=event_data)[,-1]
  max_levels <- max(xx)
  LEVELS <- c(length(bat_ids), length(pit_ids), length(stad_ids))
  sum_levels = sum(LEVELS)
  ans <- list(N=dim(xx)[[1]],
              D=dim(xx)[[2]],
              K=length(unique(ev$outcome)),
              LEVELS=LEVELS,
              x=xx,
              y=event_data$outcome,
              MAX_LEVEL=max_levels,
              SUM_LEVELS=sum_levels,
              ev=event_data)
}

initialize_with_lme4 <- function(model_df,
                                 frm=as.formula('outcome ~ (1|bid) + (1|pid) + (1|sid)')) {

  mods <- list()
  unique_outcomes <- unique(model_df$outcome) %>% sort()
  for (u in unique_outcomes) {
    cat(sprintf('fitting model for outcome: %d \n', u))
    cc <- which(model_df$outcome == u)
    stopifnot(length(cc) > 0)
    tmp <- model_df
    tmp[cc,]$outcome <- 1
    tmp[-cc,]$outcome <- 0
    glmer_mod <- glmer(frm, data=tmp,
                       nAGQ = 0,
                       family = binomial,
                       control=glmerControl(optimizer = "nloptwrap")
    )
    mods[[as.character(u)]] <- glmer_mod
  }
mods

}

update_ans <- function(ev, mods, ans) {
  nl <- length(names(mods))

  ans$theta <- matrix(rep(0, ans$D * ans$K), ncol=ans$D)
  for (i in seq_along(names(mods))) {
    ichar <- names(mods)[[i]]
    mod <- mods[[ichar]]
    thetas <- sapply(mod@theta, max, 1e-6)
    ans$theta[i,] <- thetas
  }

  ans
}

do_fit <- function(ans, warmup=100, iter=500, init=0, seed=10101) {
  stan(file='data/multinom_ravel_init.stan',
       data=ans,
       iter=iter,
       warmup=warmup,
       init=init,
       seed=seed,
       cores=4)
}
