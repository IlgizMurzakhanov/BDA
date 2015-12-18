data {
  int<lower=0> n_subjects;
  int<lower=0> n_data;
  int<lower=0,upper=1> LaterOptionChosen[n_data];
  real X1[n_data];
  real X2[n_data];
  real T1[n_data];
  real T2[n_data];
  int Subject[n_data];
}
parameters {
  real delta;
  real a;
  real mu_d;
  real mu_a;
  real<lower=0> tau_d;
  real<lower=0> tau_a;
}
transformed parameters {
  real p[n_data];
  real z[n_data];
  for (d in 1:n_data) {
    z[d] <- X2[d] * delta^T2[d] - X1[d] * delta^T1[d];
    p[d] <- inv_logit(a * z[d]);
  }
}
model {
  mu_d ~ beta(20, 20);
  mu_a ~ uniform(0.0001, 500);
  tau_d ~ uniform(0.0001, 1);
  tau_a ~ uniform(0.001, 100);
  delta ~ normal(mu_d, tau_d);
  a ~ normal(mu_a, tau_a);
  for (d in 1:n_data)
    LaterOptionChosen[d] ~ bernoulli(p[d]);
}