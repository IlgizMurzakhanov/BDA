data {
  int J;
  int n[J];
  real x[J];
  int y[J];
}
parameters {
  real alpha;
  real beta;
}
transformed parameters {
  real theta[J];
  for (j in 1:J)
    theta[j] <- inv_logit(alpha + beta * x[j]);
}
model {
  y ~ binomial(n, theta);
}
generated quantities {
  real LD50;
  LD50 <- -alpha/beta;
}