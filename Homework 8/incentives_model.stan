data {
  int<lower=0> I;
  int<lower=0> N_beta;
  int<lower=0> N_J;
  vector[I] J; //sid
  vector[I] N; //basen
  vector[I] pi; //r
  vector[I] inc; //I
  vector[I] m; 
  vector[I] t; 
  vector[I] f;
  vector[I] r.dif;
  vector[I] v;
  vector[I] v.dif;
  vector[I] b;
}
parameters {
  real<lower=0> sigma;
  real<lower=0> tau;
  vector[N_beta] beta;
  vector[N_J] alpha;
}
transformed parameters {
}
model {
  alpha ~ normal(0, tau);
  pi ~ normal(betaX + alpha, sigma);
  n ~ binomial(N, pi);
}