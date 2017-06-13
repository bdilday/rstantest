
data {
  int<lower=0> N;
  int<lower=1> D;
  int<lower=0> MAX_LEVEL;
  int<lower=0, upper=1> y[N];
  int<lower=0, upper=MAX_LEVEL> x[N, D];
}

transformed data {

}

parameters {
  real alpha[MAX_LEVEL, D];
  real C;
}

transformed parameters {

}

model {
  real lambda[N];

  for (n in 1:MAX_LEVEL) {
    for (d in 1:D) {
      alpha[n][d] ~ normal(0, 1);
    }
  }

  for (n in 1:N) {
    lambda[n] = C;
    for (d in 1:D) {
      lambda[n] = lambda[n] + alpha[ x[n][d]  ][d];
    }
  }

  y ~ bernoulli_logit(lambda);



}
