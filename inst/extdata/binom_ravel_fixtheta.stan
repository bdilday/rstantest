
data {
  int<lower=0> N;
  int<lower=1> D;
  int<lower=0> LEVELS[D];
  real<lower=0> RANEF_SIGMA[D];
  int<lower=0> SUM_LEVELS;
  int<lower=0> MAX_LEVEL;
  int<lower=0, upper=1> y[N];
  int<lower=0, upper=MAX_LEVEL> x[N, D];
}

transformed data {
  real Y8[N];
  real MEAN_Y;
  real MEAN_C;
  int CU_LEVELS[2, D];
  CU_LEVELS[1, 1] = 1;
  CU_LEVELS[2, 1] = LEVELS[1];
  for (d in 2:D) {
    CU_LEVELS[1, d] = CU_LEVELS[1, d-1] + LEVELS[d-1];
    CU_LEVELS[2, d] = CU_LEVELS[2, d-1] + LEVELS[d];
  }

  for (n in 1:N) {
    Y8[n] = 1.0 * y[n];
  }
  MEAN_Y = mean(Y8);
  MEAN_C = log(MEAN_Y/(1-MEAN_Y));
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

  C ~ normal(MEAN_C, 2);
  for (i in 1:SUM_LEVELS) {
    for (d in 1:D) {
      if (i >= CU_LEVELS[1,d] && i <= CU_LEVELS[2, d]) {
      ALPHAX[i] ~ normal(0, RANEF_SIGMA[d]);
      }
    }
  }

  for (n in 1:N) {
    lambda[n] = C;
    for (d in 1:D) {
      lambda[n] = lambda[n] + ALPHAX[ x[n][d]  ];
    }
  }

  y ~ bernoulli_logit(lambda);

}
