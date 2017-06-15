
data {
  int<lower=2> K;
  int<lower=1> N;
  int<lower=1> D;
  int<lower=1, upper=K> y[N];
  vector[D] x[N];
}

transformed data {

}

parameters {
  matrix[K, D] ALPHAX;
}

transformed parameters {

}

model {

  for (k in 1:K) {
    for (d in 1:D) {
      ALPHAX[k, d] ~ normal(0, 1);
    }
  }

  for (n in 1:N) {
    y[n] ~ categorical_logit( (x[n]' * ALPHAX)' );
    }

}
