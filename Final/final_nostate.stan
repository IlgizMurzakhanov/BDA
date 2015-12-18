data {
  int<lower=0> n_data;
  int<lower=0> n_states;
  int<lower=0> state[n_data];
  int<lower=0> count[n_data];
  int<lower=0,upper=1> married[n_data];
  int<lower=0> n_dem[n_data];
  real<lower=0> pct_married[n_states];
}
parameters {
  real beta_I;
  real beta_M;
  vector[n_states] beta_state;
  real<lower=0> sigma_I;
  real<lower=0> sigma_M;
  real<lower=0> sigma_state;
}
transformed parameters {
  vector[n_data] p;
  for (i in 1:n_data)
    p[i] <- beta_I + beta_M * married[i] + beta_state[state[i]];
}
model {
  beta_I ~ normal(0, sigma_I);
  beta_M ~ normal(0, sigma_M);
  beta_state ~ normal(0, sigma_state);
  sigma_I ~ normal(0, 5);
  sigma_M ~ normal(0, 5);
  sigma_state ~ normal(0, 5);
  for (j in 1:n_data)
    n_dem[j] ~ binomial_logit(count[j], p[j]);
}
generated quantities {
  vector[n_data] p_pred;
  vector[n_states] p_mar;
  vector[n_states] p_not;
  vector[n_states] obama_pct;
  
  for (i in 1:n_data)
    p_pred[i] <- inv_logit(p[i]) * 100;
  for (i in 1:n_states){
    p_mar[i] <- beta_I + beta_M + beta_state[state[i]];
    p_not[i] <- beta_I + beta_state[state[i]];
    obama_pct[i] <- (pct_married[i] * inv_logit(p_mar[i]) + (1 - pct_married[i]) * inv_logit(p_not[i])) * 100;
  }
}