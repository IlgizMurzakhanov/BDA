data {
  int<lower=0> n_subjects;
  int<lower=0> n_data;
  int<lower=0,upper=1> LaterOptionChosen[n_data];
  real G[n_data];
  real R[n_data];
  real D[n_data];
  real T[n_data];
  int Subject[n_data];
}
parameters {
  vector[5] mu;
  vector<lower=0>[5] sigma;
  vector[5] beta[n_subjects];
}
transformed parameters {
  real p[n_data];
  for (d in 1:n_data) {
    p[d] <- beta[Subject[d], 1] + beta[Subject[d], 2] * G[d] + beta[Subject[d], 3] * R[d] + beta[Subject[d], 4] * D[d] + beta[Subject[d], 5] * T[d];
  }
}
model {
  mu ~ normal(0, 100);
  sigma ~ uniform(0, 100);
  for (s in 1:n_subjects)
    beta[s] ~ normal(mu, sigma);
  for (d in 1:n_data)
    LaterOptionChosen[d] ~ bernoulli_logit(p[d]);
}
generated quantities {
  vector[n_data] log_lik;

  for (d in 1:n_data) 
    log_lik[d] <- bernoulli_logit_log(LaterOptionChosen[d], p[d]);
}