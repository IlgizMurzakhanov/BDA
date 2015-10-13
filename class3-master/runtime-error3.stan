transformed data {
  real y[10];
  real x[10];
  for (n in 1:10)
    x[n] <- n;
  y[1] <- 1.3898432;
  y[2] <- 1.3787594;
  y[3] <- 0.7853001;
  y[4] <- 5.1249309;
  y[5] <- 4.9550664;
  y[6] <- 5.9838097;
  y[7] <- 7.9438362;
  y[8] <- 8.8212212;
  y[9] <- 9.5939013;
  y[10] <- 10.9189774;
}
parameters {
  real alpha;
  real beta;
  real<lower = 0> sigma_sq;
}
transformed parameters {
  real<lower = 0> sigma;
}
model {
  sigma_sq ~ cauchy(0, 1);
  y ~ normal(0, sigma);
}
