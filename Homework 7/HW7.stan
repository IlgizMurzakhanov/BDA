data {
  int<lower=1> N;
  vector[N] ageBin;
  vector[N] gks;
  vector[N] gender;
}
transformed data {
  vector[N] mu;
  for (i in 1:N) 
    mu[i] <- 0;
}
parameters {
  real<lower=0> tau_sq;
  real<lower=0> l_sq;
  real<lower=0> sigma_sq;
}
model {
  matrix[N,N] Sigma;

  // off-diagonal elements
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      Sigma[i,j] <- tau_sq * exp(-pow(ageBin[i] - ageBin[j],2)/l_sq);
      Sigma[j,i] <- Sigma[i,j];
    }
  }

  // diagonal elements
  for (k in 1:N)
    Sigma[k,k] <- tau_sq + sigma_sq; // + jitter

  tau_sq ~ cauchy(0,5);
  l_sq ~ cauchy(0,5);
  sigma_sq ~ cauchy(0,5);

  gks ~ multi_normal(mu,Sigma);
}