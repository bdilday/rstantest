
data {
  int<lower=2> K;
  int<lower=1> N;
  int<lower=1> D;
  int<lower=0> LEVELS[D];
  int<lower=0> SUM_LEVELS;
  int<lower=1, upper=K> y[N];
  int<lower=1> x[N, D];
}

transformed data {

}

parameters {
  real ALPHAX[K, SUM_LEVELS];
  vector[K] C;
}

transformed parameters {

}

model {

  vector[K] lambda[N];

  for (k in 1:K) {
    C[k] ~ normal(0, 1);
    for (i in 1:SUM_LEVELS) {
      ALPHAX[k, i] ~ normal(0, 1);
    }
  }

  for (n in 1:N) {
    for (k in 1:K) {
      lambda[n][k] = C[k];
      for (d in 1:D) {
        lambda[n][k] = lambda[n][k] + ALPHAX[k,  x[n][d]  ];
    //    print(n, " ", d, " ", k, " ", x[n][d], " ", ALPHAX[k, x[n][d] ], " ", lambda[n][k]);
      }
    }
  }


  for (n in 1:N) {
    y[n] ~ categorical_logit(lambda[n]);
  }

}
