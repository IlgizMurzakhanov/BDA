data {
  int<lower=1> N_data;     
  vector[N_data] ageBin_data; 
  vector[N_data] gks_data;
  int<lower=1> N_sim;
  vector[N_sim] ageBin_sim;
  vector[N_data] sigma_sq;
  real l_sq;
  real tau_sq;
}
transformed data {
  int<lower=1> N;
  vector[N_data+N_sim] ageBin;
  vector[N_data+N_sim] mu;
  cov_matrix[N_data+N_sim] Sigma;
  N <- N_data + N_sim;
  for (n in 1:N_data) ageBin[n] <- ageBin_data[n];
  for (n in 1:N_sim) ageBin[N_data + n] <- ageBin_sim[n];
  for (i in 1:N) mu[i] <- 0;
  for (i in 1:N) 
    for (j in 1:N)
      Sigma[i,j] <- tau_sq * exp(-pow(ageBin[i] - ageBin[j],2)/l_sq) + if_else(i==j, sigma_sq[j], 0.0);
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