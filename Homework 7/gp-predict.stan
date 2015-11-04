data {
  int<lower=1> N_data;     
  vector[N_data] ageBin_data; 
  vector[N_data] gender_data; 
  vector[N_data] gks_data;
  int<lower=1> N_sim;
  vector[N_sim] ageBin_sim;
  vector[N_sim] gender_sim; 
  vector[N_data] sigma_sq;
  real l_sq_m;
  real tau_sq_m;
  real l_sq_f;
  real tau_sq_f;
}
transformed data {
  int<lower=1> N;
  vector[N_data+N_sim] ageBin;
  vector[N_data+N_sim] gender;
  vector[N_data+N_sim] mu;
  matrix[N_data+N_sim, N_data+N_sim] Sigma_m;
  matrix[N_data+N_sim, N_data+N_sim] Sigma_f;
  cov_matrix[N_data+N_sim] Sigma;
  N <- N_data + N_sim;
  for (n in 1:N_data) ageBin[n] <- ageBin_data[n];
  for (n in 1:N_sim) ageBin[N_data + n] <- ageBin_sim[n];
  for (n in 1:N_data) gender[n] <- gender_data[n];
  for (n in 1:N_sim) gender[N_data + n] <- gender_sim[n];
  for (i in 1:N) mu[i] <- 0;
  for (i in 1:N) {
    for (j in 1:N_data) {
      Sigma_m[i,j] <- (1 - gender[i]) * (1 - gender[j]) * 
        (tau_sq_m * exp(-pow(ageBin[i] - ageBin[j],2)/l_sq_m) + if_else(i==j, sigma_sq[j], 0.0));
      Sigma_f[i,j] <- gender[i] * gender[j] * 
        (tau_sq_f * exp(-pow(ageBin[i] - ageBin[j],2)/l_sq_f) + if_else(i==j, sigma_sq[j], 0.0));
    }
    for (j in N_data+1:N) {
      Sigma_m[i,j] <- (1 - gender[i]) * (1 - gender[j]) * 
        (tau_sq_m * exp(-pow(ageBin[i] - ageBin[j],2)/l_sq_m) + if_else(i==j, .0001, 0.0));
      Sigma_f[i,j] <- gender[i] * gender[j] * 
        (tau_sq_f * exp(-pow(ageBin[i] - ageBin[j],2)/l_sq_f) + if_else(i==j, .0001, 0.0));
    }
  }
  Sigma <- Sigma_m + Sigma_f;
}

parameters {
  vector[N_sim] gks_sim;
}
model {
  vector[N] gks;
  for (n in 1:N_data) gks[n] <- gks_data[n];
  for (n in 1:N_sim) gks[N_data + n] <- gks_sim[n];

  gks ~ multi_normal(mu,Sigma);
}