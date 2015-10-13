data {
  real<lower=0> r; //ball radius
  real<lower=0> R; //hole radius
  int<lower=0> I; //#data points
  real<lower=R-r> x[I]; // distance(feet)
  int<lower=0> n[I]; // #tries
  int<lower=0> y[I]; // #successes
}
parameters {
  real<lower=0> sigma;
}
transformed parameters {
  real<lower=0,upper=1> theta[I];
  real as[I];
  for (i in 1:I)
    as[i] <- asin((R - r) / x[i])  / sigma;
  for (i in 1:I)
    theta[i] <- 2 * normal_cdf(as[i], 0, 1) - 1;
}
model {
  y ~ binomial(n, theta);
}
