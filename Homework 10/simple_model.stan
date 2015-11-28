data {
  int<lower=0> N;
  int<lower=0> N_years;
  int<lower=0> N_genders;
  real V[N];
  real rate[N];
  vector[N] age;
  int year[N];
  int gender[N]; 
  int decades[N];
}
parameters {
  real<lower=0> sigma_sq;
  real constant;
  real beta_a;
  vector[N_years] beta_y;
  vector[N_genders] beta_g;
  vector[N_years] beta_y1;
  vector[N_years] beta_y2;
  vector[N_years] beta_y3;
  real<lower=0> tau_y;
}
transformed parameters {
  vector[N_years] beta_yd[3];
  beta_yd[1] <- beta_y1;
  beta_yd[2] <- beta_y2;
  beta_yd[3] <- beta_y3;
}
model {
  beta_y ~ normal(0, tau_y);
  beta_g ~ normal(0, 100);
  beta_a ~ normal(30, 30);
  beta_y1 ~ normal(0, 100);
  beta_y2 ~ normal(0, 100);
  beta_y3 ~ normal(0, 100);
  for(i in 1:N){
    rate[i] ~ normal(beta_a * age[i] + beta_y[year[i]] + beta_g[gender[i]] + beta_yd[decades[i], year[i]] + constant, sigma_sq + V[i]);
  }
}
