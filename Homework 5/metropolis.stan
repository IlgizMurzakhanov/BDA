data {
  int n;
  int p;
  matrix[n, p] X;
  int y[n];
}
parameters {
  vector[p] beta;
}
model {
  y ~ poisson(exp(X * beta));
}