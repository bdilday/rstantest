
data {
  int<lower=0> N;
  int<lower=1> D;
  int<lower=0> LEVELS[D];
  int<lower=0> SUM_LEVELS;
  int<lower=0> MAX_LEVEL;
  int<lower=0, upper=1> y[N];
  int<lower=0, upper=MAX_LEVEL> x[N, D];
}

transformed data {
  int CU_LEVELS[2, D];
  CU_LEVELS[1, 1] = 1;
  CU_LEVELS[2, 1] = LEVELS[1];
  for (d in 2:D) {
    CU_LEVELS[1, d] = CU_LEVELS[1, d-1] + LEVELS[d-1];
    CU_LEVELS[2, d] = CU_LEVELS[2, d-1] + LEVELS[d-1];
  }
}

parameters {
  real ALPHAX[SUM_LEVELS];
//  real alpha[MAX_LEVEL, D];
  real C;
}

transformed parameters {

}

model {
  real lambda[N];

  for (i in 1:SUM_LEVELS) {
  ALPHAX[i] ~ normal(0, 1);
  }

  for (n in 1:N) {
    lambda[n] = C;
    for (d in 1:D) {
      lambda[n] = lambda[n] + ALPHAX[ x[n][d]  ];
    }
  }

  y ~ bernoulli_logit(lambda);

}
