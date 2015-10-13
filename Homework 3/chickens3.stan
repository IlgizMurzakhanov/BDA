data{
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
  real treat_effect_E[J];
  real treat_effect_S[J];
}
transformed parameters {
  vector[J] sigma_E;
  vector[J] sigma_S;
  vector[J] std_E;
  vector[J] std_S;

  for (j in 1:J) {
    std_E[j] <- (sqrt(E_N[j]) * E_se[j]);
    sigma_E[j] <- pow(std_E[j], 2);
    std_S[j] <- (sqrt(S_N[j]) * S_se[j]);
    sigma_S[j] <- pow(std_S[j], 2);
  }
}
model {
  for (j in 1:J) {
    treat_effect_E[j] ~ normal(1, 1);
    treat_effect_S[j] ~ normal(1, 1);
    E_mean[j] ~ normal(treat_effect_E[j], sigma_E[j]);
    S_mean[j] ~ normal(treat_effect_S[j], sigma_S[j]);
  }
}
generated quantities {
  real treat_effect[J];
  for (j in 1:J) 
    treat_effect[j] <- treat_effect_E[j]/treat_effect_S[j];
}
