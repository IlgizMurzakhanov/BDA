data {
  int<lower=0> N;
  real V[N];
  real rate[N];
  vector[N] age;
}
parameters {
  real<lower=0> sigma_sq;
  real<lower=0> constant;
  real beta_a;
}
model {
  beta_a ~ normal(0, 100);
  for(i in 1:N)
    rate[i] ~ normal(beta_a * age + constant, sigma_sq + V[i]);
}