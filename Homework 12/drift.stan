data {
  int<lower=0> n_subjects;
  int<lower=0> n_data;
  int<lower=0,upper=1> LaterOptionChosen[n_data];
  real DriftD[n_data];
  real DriftR[n_data];
  real DriftI[n_data];
  real DriftT[n_data];
  int Subject[n_data];
}
parameters {
  vector[5] alpha;
  vector<lower=0>[5] tau;
  vector[5] beta[n_subjects+1];
}
transformed parameters {
  real p[n_data];
  for (d in 1:n_data) {
    p[d] <- beta[Subject[d], 1] + beta[Subject[d], 2] * DriftD[d] + beta[Subject[d], 3] * DriftR[d] + beta[Subject[d], 4] * DriftI[d] + beta[Subject[d], 5] * DriftT[d];
  }
}
model {
  alpha ~ normal(0, 100);
  for (s in 1:n_subjects)
    beta[s] ~ normal(alpha, tau);
  for (d in 1:n_data)
    LaterOptionChosen[d] ~ bernoulli_logit(p[d]);
}