data {
  int<lower=1> N_data;
  vector[N_data] ageBin_data;
  vector[N_data] gks_data;
  vector[N_data] gender_data;
  vector[N_data] sigma_sq;
}
transformed data {
  vector[N_data] mu;
  for (i in 1:N_data) 
    mu[i] <- 0;
}
parameters {
  real<lower=0> tau_sq_m;
  real<lower=0> l_sq_m;
  real<lower=0> tau_sq_f;
  real<lower=0> l_sq_f;
}
model {
  matrix[N_data,N_data] Sigma_m;
  matrix[N_data,N_data] Sigma_f;
  matrix[N_data,N_data] Sigma;
  
  tau_sq_m ~ cauchy(0,5);
  l_sq_m ~ cauchy(0,5);
  tau_sq_f ~ cauchy(0,5);
  l_sq_f ~ cauchy(0,5);

  // off-diagonal elements
  for (i in 1:N_data) {
    for (j in 1:N_data) {
      Sigma_m[i,j] <- (1 - gender_data[i]) * (1 - gender_data[j]) * 
        (tau_sq_m * exp(-pow(ageBin_data[i] - ageBin_data[j],2)/l_sq_m) + if_else(i==j, sigma_sq[i], 0.0));
      Sigma_f[i,j] <- gender_data[i] * gender_data[j] * 
        (tau_sq_f * exp(-pow(ageBin_data[i] - ageBin_data[j],2)/l_sq_f) + if_else(i==j, sigma_sq[i], 0.0)); 
    }
  }
  
  Sigma <- Sigma_m + Sigma_f;

  gks_data ~ multi_normal(mu,Sigma);
}
