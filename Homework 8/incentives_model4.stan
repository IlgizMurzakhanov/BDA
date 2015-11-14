data {
  int<lower=0> N_I;
  int<lower=0> N_J;
  int J[N_I]; //sid
  int N[N_I]; //basen
  vector[N_I] y; //r
  vector[N_I] V;
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
  real<lower=0> tau_b_sq;
  real<lower=0> tau_bv_sq;
  real beta_m;
  real beta_b;
  real beta_I;
  real beta_mb;
  real beta_Iv;
  real beta_It;
  real beta_If;
  real beta_Im;
  real beta_Ib;
  real beta_Ivt;
  real beta_Ivb;
  real beta_Itb;
  real beta_Ivf;
  real beta_Ivm;
  real beta_Itf;
  real beta_Itm;
  real beta_Ifb;
  real constant;
  vector[N_J] alpha;
  vector[N_I] pi;
}
transformed parameters {
  vector[N_I] XB;
  for (i in 1:N_I)
    XB[i] <- beta_I * I[i] + beta_b * b[i] + beta_m * m[i] + beta_mb * m[i] * b[i] + beta_Iv * I[i] * v[i] + beta_It * I[i] * t[i] + beta_If * I[i] * f[i] + beta_Im * I[i] * m[i] + beta_Ib * I[i] * b[i] + beta_Ivt * I[i] * v[i] * t[i] + beta_Ivb * I[i] * v[i] * b[i] + beta_Ivf * I[i] * v[i] * f[i] + beta_Ivm * I[i] * v[i] * m[i] + + beta_Itm * I[i] * t[i] * m[i] + beta_Itf * I[i] * t[i] * f[i] + beta_Itm * I[i] * t[i] * m[i] + beta_Ifb * I[i] * f[i] * b[i] + constant;
}
model {
  beta_m ~ normal(0, tau_b_sq);
  beta_b ~ normal(0, tau_b_sq);
  beta_I ~ normal(0, tau_b_sq);
  beta_mb ~ normal(0, tau_b_sq);
  beta_Iv ~ normal(0, tau_bv_sq);
  beta_It ~ normal(0, tau_b_sq);
  beta_If ~ normal(0, tau_b_sq);
  beta_Im ~ normal(0, tau_b_sq);
  beta_Ib ~ normal(0, tau_b_sq);
  beta_Ivt ~ normal(0, tau_bv_sq);
  beta_Ivb ~ normal(0, tau_bv_sq);
  beta_Itb ~ normal(0, tau_b_sq);
  beta_Ivf ~ normal(0, tau_bv_sq);
  beta_Ivm ~ normal(0, tau_bv_sq);
  beta_Itf ~ normal(0, tau_b_sq);
  beta_Itm ~ normal(0, tau_b_sq);
  beta_Ifb ~ normal(0, tau_b_sq);
  alpha ~ normal(0, tau_sq);
  for(i in 1:N_I)
    y[i] ~ normal(XB[i] + alpha[J[i]], sigma_sq + V[i]);
}
