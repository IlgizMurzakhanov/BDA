data {
  int<lower = 0> J;
  int hz[J];
  int S_N[J];
  int E_N[J];
  vector[J] S_mean;
  vector[J] E_mean;
  vector[J] S_se;
  vector[J] E_se;
}
parameters {
  real treat_effect[J];
  real a;
}
transformed parameters {
  vector[J] sigma;
  vector[J] std;

  for (j in 1:J) {
    std[j] <- (sqrt(E_N[j]) * E_se[j]);
    sigma[j] <- pow(std[j], 2);
  }
}
model {
  for (j in 1:J) {
    E_mean[j] ~ normal(treat_effect[j], sigma[j]);
  }
}
