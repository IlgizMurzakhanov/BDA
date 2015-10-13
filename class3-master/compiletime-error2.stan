data {
  int M;
  real x[N, M];
  real y[N];
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, x);
}
