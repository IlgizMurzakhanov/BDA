data {
  int<lower=0> N;
  int<lower=0> N_years;
  int<lower=0> N_genders;
  int<lower=0> N_regions;
  real V[N];
  real rate[N];
  vector[N] age;
  int year[N]; 
  int gender[N];
  int region[N];
}
parameters {
  real<lower=0> sigma_sq;
  real constant;
  real beta_a;
  vector[N_genders] beta_g;
  vector[N_years] beta_y;
  vector[N_regions] beta_r;
  real<lower=0> tau_y;
  real<lower=0> tau_g;
  real<lower=0> tau_r;
  vector[N] pi;
}
model {
  beta_y ~ normal(0, tau_y);
  beta_g ~ normal(0, tau_g);
  beta_r ~ normal(0, tau_r);
  beta_a ~ normal(0, 10);
  for(i in 1:N)
    rate[i] ~ normal(beta_a * age[i] + beta_y[year[i]] + beta_g[gender[i]] + beta_r[region[i]] + constant, sigma_sq + V[i]);
}