data {
  int<lower=0> n_subjects;
  int<lower=0> n_data;
  int<lower=0> n_conds;
  real<lower=0,upper=1> epsilon;
  int<lower=0,upper=1> LaterOptionChosen[n_data];
  real X1[n_data];
  real X2[n_data];
  real T1[n_data];
  real T2[n_data];
  int Subject[n_data];
  int conds[n_subjects];
}
parameters {
  real<lower=0, upper=1> delta[n_subjects];
  real a[n_subjects];
  
  real<lower=0, upper=1> mu_d[n_conds];
  real<lower=0> kappa_d[n_conds];
  real mu_a[n_conds];
  real<lower=0> sigma_a[n_conds];
  
  real<lower=0, upper=1> mu_d_hyper;
  real<lower=0> kappa_d_hyper;
  real mu_a_hyper;
  real<lower=0> sigma_a_hyper;
}
transformed parameters {
  real p[n_data];
  real z[n_data];
  real alpha_d[n_conds];
  real beta_d[n_conds];
  real alpha_d_hyper;
  real beta_d_hyper;
  for (d in 1:n_data) {
    z[d] <- X2[d] * delta[Subject[d]]^T2[d] - X1[d] * delta[Subject[d]]^T1[d];
    //homothetic:
    //z[d] <- log(z[d]);
    p[d] <- inv_logit(a[Subject[d]] * z[d]);
    //bounded:
    //p[d] <- epsilon * 0.5 + (1 - epsilon) * p[d];
  }
  
  for (c in 1:n_conds) {
    alpha_d[c] <- mu_d[c] * kappa_d[c];
    beta_d[c] <- kappa_d[c] - alpha_d[c];
  }
  
  alpha_d_hyper <- mu_d_hyper * kappa_d_hyper;
  beta_d_hyper <- kappa_d_hyper - alpha_d_hyper;
}
model {
  mu_d_hyper ~ beta(1, 1);
  kappa_d_hyper ~ uniform(0, 60);
  mu_a_hyper ~ uniform(0.0001, 50);
  sigma_a_hyper ~ uniform(0, 40);
  for (c in 1:n_conds) {
    mu_d[c] ~ beta(alpha_d_hyper, beta_d_hyper);
    kappa_d[c] ~ uniform(0, 60);
    mu_a[c] ~ normal(mu_a_hyper, sigma_a_hyper);
    sigma_a[c] ~ uniform(0, 25);
  }
  for (s in 1:n_subjects) {
    delta[s] ~ beta(alpha_d[conds[s]], beta_d[conds[s]]);
    a[s] ~ normal(mu_a[conds[s]], sigma_a[conds[s]]);
  }
  for (d in 1:n_data)
    LaterOptionChosen[d] ~ bernoulli(p[d]);
}