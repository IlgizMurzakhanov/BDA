data {
  int n;
  int x[n];
  real u[n];
  int y[n];
}
parameters {
  real alpha;
  real beta;
}
model {
  for (i in 1:n) {
    y[n] ~ poisson(u[n] * exp(alpha + (beta * x[n])));
  }
}