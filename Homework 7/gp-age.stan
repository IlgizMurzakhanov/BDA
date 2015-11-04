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
  real<lower=0> tau_sq;
  real<lower=0> l_sq;
}
model {
  matrix[N_data,N_data] Sigma;

  for (i in 1:N_data) {
    for (j in 1:N_data) {
      Sigma[i, j] <- tau_sq * exp(-pow(ageBin_data[i] - ageBin_data[j],2)/l_sq) + if_else(i==j, sigma_sq[i], 0.0);
    }
  }
  
  tau_sq ~ cauchy(0,5);
  l_sq ~ cauchy(0,5);

  gks_data ~ multi_normal(mu,Sigma);
}