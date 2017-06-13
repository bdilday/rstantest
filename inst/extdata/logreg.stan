
data {
int<lower=0> N;
int<lower=1> D;
int<lower=0, upper=1> y[N];
matrix[N,D] x;
}

transformed data {

}

parameters {
vector[D] beta;
}

transformed parameters {

}

model {
  for (n in 1:N) {
  y[n] ~ bernoulli_logit(x[n] * beta);
  }

}
