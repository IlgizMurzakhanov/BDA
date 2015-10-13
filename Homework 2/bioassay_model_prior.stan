data {
  int J;
  int n[J];
  real x[J];
  int y[J];
  vector[2] mu;
  cov_matrix[2] Sigma;
}
parameters {
  vector[2] pars;
}
transformed parameters {
  real alpha;
  real beta;
  real theta[J];
  alpha <- pars[1];
  beta <- pars[2];
  for (j in 1:J)
    theta[j] <- inv_logit(alpha + beta * x[j]);
}
model {
  pars ~ multi_normal(mu, Sigma);
  y ~ binomial(n, theta);
}
generated quantities {
  real LD50;
  LD50 <- -alpha/beta;
}