data {
  int J;
  int n[J];
  real x[J];
  int y[J];
  real r;
  real R;
}
parameters {
  real<lower=0> sigma;
}
model {
  real p[J];
  for (j in 1:J)
    p[j] <- 2*Phi(asin((R-r)/x[j]) / sigma) - 1;
  y ~ binomial(n, p);
}
generated quantities {
  real sigma_degrees;
  sigma_degrees <- (180/pi())*sigma;
}

