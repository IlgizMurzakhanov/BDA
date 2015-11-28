data {
  int<lower=0> N;
  int<lower=0> N_years;
  int<lower=0> N_genders;
  real V[N];
  vector[N] rate;
  vector[N] age;
  int year[N];
  int gender[N]; 
  int decades[N];
}
transformed data {
  vector[N] mu;
  for (i in 1:N) 
    mu[i] <- 0;
}
parameters {
  real<lower=0> sigma_sq;
  real constant;
  real<lower=0> tau_sq_a;
  real<lower=0> l_sq_a;
  real<lower=0> tau_sq_y;
  real<lower=0> l_sq_y;
  real<lower=0> sigma_sq_ay;
  vector[N] beta_ay;
}
model {
  matrix[N,N] Sigma;
  matrix[N,N] Sigma_a;
  matrix[N,N] Sigma_y;
  // off-diagonal elements
  for (i in 1:N) {
    for (j in 1:N) {
      Sigma_a[i,j] <- (tau_sq_a * exp(-pow(age[i] - age[j],2)/l_sq_a) + if_else(i==j, sigma_sq + V[i], 0.0));
      Sigma_y[i,j] <- (tau_sq_y * exp(-pow(year[i] - year[j],2)/l_sq_y) + if_else(i==j, 0.0, 0.0));
    }
  }
  Sigma <- Sigma_a + Sigma_y;
  
  tau_sq_a ~ normal(0,5);
  l_sq_a ~ normal(0,5);
  tau_sq_y ~ normal(0,5);
  l_sq_y ~ normal(0,5);
  
  //beta_ay ~ multi_normal(mu,Sigma);
  //for(i in 1:N){
    //rate[i] ~ normal(beta_a * age[i] + beta_y[year[i]] + beta_g[gender[i]] + beta_yd[decades[i], year[i]] + constant, sigma_sq + V[i]);
  //}
  rate ~ multi_normal(mu,Sigma);
}