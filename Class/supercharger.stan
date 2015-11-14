data {
  int N;
  vector[N] x;
  vector[N] log_y;
}
parameters {
  real a;
  real b;
  real sigma;
}
model {
  log_y ~ normal(a + b * x, sigma);
}
generated quantities {
  vector[N] log_y_pred;
  for(i in 1:N) {
    log_y_pred[i] <- normal_rng(a + b * x[i], sigma);
  }
}