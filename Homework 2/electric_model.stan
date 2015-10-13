data {
  int<lower=0> N;
  int<lower=0> G;
  int<lower=0> C;
  vector[N] post;
  vector[N] treatment;
  vector[N] pre;
  int grade[N];
  vector[N] city;
}
parameters {
  vector[G] a;
  vector[G] b;
  vector[G] c;
  vector[G] d;
  real mu_a;
  real mu_b;
  real mu_c;
  real mu_d;
  real<lower=0,upper=100> sigma_a;
  real<lower=0,upper=100> sigma_b;
  real<lower=0,upper=100> sigma_c;
  real<lower=0,upper=100> sigma_d;
  real<lower=0,upper=100> sigma_y[G];
}
transformed parameters {
  vector[N] y_hat;
  vector[N] sigma_y_hat;  

  for (i in 1:N) {
    y_hat[i] <- a[grade[i]] + b[grade[i]] * treatment[i] + c[grade[i]] * pre[i] + d[grade[i]] * city[i];
    sigma_y_hat[i] <- sigma_y[grade[i]];
  }
}
model {
  a ~ normal(mu_a, sigma_a);
  b ~ normal(mu_b, sigma_b);
  c ~ normal(mu_c, sigma_c);
  d ~ normal(mu_d, sigma_d);
  post ~ normal(y_hat, sigma_y_hat);
}