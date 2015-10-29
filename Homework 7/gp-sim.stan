data {
  int<lower=1> N;
  vector[N] ageBin;
  real l_sq;
  real tau_sq;
}
transformed data {
  vector[N] mu;
  cov_matrix[N] Sigma;
  for (i in 1:N) 
    mu[i] <- 0;
  for (i in 1:N) 
    for (j in 1:N)
      Sigma[i,j] <- tau_sq * exp(-pow(ageBin[i] - ageBin[j],2)/l_sq) + if_else(i==j, .05, 0.0);
}
parameters {
  vector[N] gks;
}
model {
  gks ~ multi_normal(mu,Sigma);
}