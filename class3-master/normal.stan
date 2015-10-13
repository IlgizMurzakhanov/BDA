data {
  int<lower = 0> N;
  int<lower = 0> M;
  vector[N] y;
  matrix[N, M] X;
}
parameters {
  real alpha;
  vector[M] beta;
  real<lower = 0> sigma;
}
model {
  y ~ normal(alpha + X * beta, sigma);
}
