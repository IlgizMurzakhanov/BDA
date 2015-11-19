data {
  int<lower=0> N;
  int<lower=0> N_years;
  int<lower=0> N_genders;
  int<lower=0> N_regions;
  int population[N];
  int deaths[N];
  vector[N] age;
  int year[N]; 
  int gender[N];
  int region[N];
}
parameters {
  real<lower=0> sigma_sq;
  real<lower=0> constant;
  real beta_a;
  vector[N_genders] beta_g;
  vector[N_years] beta_y;
  vector[N_regions] beta_r;
  real tau_y;
  real tau_g;
  real tau_r;
  vector[N] pi;
}
model {
  beta_y ~ normal(0, tau_y);
  beta_g ~ normal(0, tau_g);
  beta_r ~ normal(0, tau_r);
  beta_a ~ normal(0, 10);
  for(i in 1:N)
    pi[i] ~ normal(beta_a * age + beta_y[year[i]] + beta_g[gender[i]] + beta_r[region[i]], sigma_sq);
  deaths ~ binomial_logit(population, pi);
}