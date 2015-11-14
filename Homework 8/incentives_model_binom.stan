data {
  int<lower=0> N_I;
  int<lower=0> N_J;
  int J[N_I]; //sid
  int N[N_I]; //basen
  int n[N_I]; //r
  vector[N_I] I;
  vector[N_I] m; 
  vector[N_I] t; 
  vector[N_I] f;
  vector[N_I] v;
  vector[N_I] b;
}
parameters {
  real<lower=0> sigma_sq;
  real<lower=0> tau_sq;
  real beta_m;
  real beta_t;
  real beta_f;
  real beta_v;
  real beta_b;
  real beta_I;
  real constant;
  vector[N_J] alpha;
  vector[N_I] pi;
}
transformed parameters {
  vector[N_I] XB;
  for (i in 1:N_I)
    XB[i] <- beta_I * I[i] + beta_b * b[i] + beta_m * m[i] + constant;
}
model {
  alpha ~ normal(0, tau_sq);
  for(i in 1:N_I)
    pi[i] ~ normal(XB[i] + alpha[J[i]], sigma_sq);
  n ~ binomial(N, pi);
}