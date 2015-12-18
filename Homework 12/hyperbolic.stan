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
  real<lower=0, upper=1> mu_k;
  real<lower=0> kappa_k;
  real mu_a;
  real<lower=0> sigma_a;
  real<lower=0, upper=1> k[n_subjects];
  real a[n_subjects];
}
transformed parameters {
  real p[n_data];
  real z[n_data];
  real alpha_k;
  real beta_k;
  for (d in 1:n_data) {
    z[d] <- X2[d] * (1.0 / (1.0 + k[Subject[d]] * T2[d])) - X1[d] * (1.0 / (1.0 + k[Subject[d]] * T1[d]));
    //homothetic:
    //z[d] <- log(z[d]);
    p[d] <- inv_logit(a[Subject[d]] * z[d]);
    //bounded:
    //p[d] <- epsilon * 0.5 + (1 - epsilon) * p[d];
  }
  alpha_k <- mu_k * kappa_k;
  beta_k <- kappa_k - alpha_k;
}
model {
  mu_k ~ uniform(0, 1);
  kappa_k ~ uniform(0, 100);
  mu_a ~ uniform(0.0001, 500);
  sigma_a ~ uniform(0.001, 100);
  for (s in 1:n_subjects) {
    k[s] ~ beta(alpha_k, beta_k);
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