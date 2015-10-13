data {
  int<lower = 0> N;
  int<lower = 0> M;
  vector[N] y;
  matrix[N, M] X;
}
parameters {
  real alpha;
  vector[M] beta;
  real<lower = 0> sigma;
}
model {
  // log p(y, alpha, beta, sigma)
  for (n in 1:N) {
    //increment_log_prob(log(1 / (sqrt(2 * pi()) * sigma) * exp(- square(y[n] - alpha - X[n] * beta)/(2 * square(sigma)))));
    //log parameterized
    //increment_log_prob(- log(sqrt(2 * pi()) * sigma));
    increment_log_prob(- log(sigma));
    increment_log_prob(- square(y[n] - alpha - X[n] * beta) / (2 * square(sigma)));
  }
  increment_log_prob(normal_log(y, alpha + X * beta, sigma))
  // y ~ normal(alpha + X * beta, sigma);
}

