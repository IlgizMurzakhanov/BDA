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
  real<lower=0, upper=1> mu_d;
  real<lower=0> kappa_d;
  real mu_a;
  real<lower=0> sigma_a;
  real<lower=0, upper=1> delta[n_subjects];
  real a[n_subjects];
}
transformed parameters {
  real p[n_data];
  real z[n_data];
  real alpha_d;
  real beta_d;
  for (d in 1:n_data) {
    z[d] <- X2[d] * delta[Subject[d]]^T2[d] - X1[d] * delta[Subject[d]]^T1[d];
    //homothetic:
    //z[d] <- log(z[d]);
    p[d] <- inv_logit(a[Subject[d]] * z[d]);
    //bounded:
    //p[d] <- epsilon * 0.5 + (1 - epsilon) * p[d];
  }
  alpha_d <- mu_d * kappa_d;
  beta_d <- kappa_d - alpha_d;
}
model {
  mu_d ~ uniform(0, 1);
  kappa_d ~ uniform(0, 100);
  mu_a ~ uniform(0.0001, 500);
  sigma_a ~ uniform(0.001, 100);
  for (s in 1:n_subjects) {
    delta[s] ~ beta(alpha_d, beta_d);
    a[s] ~ normal(mu_a, sigma_a);
  }
  for (d in 1:n_data)
    LaterOptionChosen[d] ~ bernoulli(p[d]);
}
generated quantities {
  vector[n_data] log_lik;

  for (d in 1:n_data) 
    log_lik[d] <- bernoulli_log(LaterOptionChosen[d], p[d]);
}