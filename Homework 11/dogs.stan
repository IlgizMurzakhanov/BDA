data {
  int<lower=0> n_dogs;
  int<lower=0> n_trials;
  int<lower=0,upper=1> y[n_dogs,n_trials];
}
parameters {
  vector[4] alpha;
  vector<lower=0>[4] tau;
  vector[4] beta[n_dogs];
}
transformed parameters {
  matrix[n_dogs,n_trials] n_avoid;
  matrix[n_dogs,n_trials] n_shock;
  matrix[n_dogs,n_trials] n_trials_since_shock;
  matrix[n_dogs,n_trials] p;
  
  for (j in 1:n_dogs) {
    n_avoid[j,1] <- 0;
    n_shock[j,1] <- 0;
    n_trials_since_shock[j,1] <- 0;
    for (t in 2:n_trials) {
      n_avoid[j,t] <- n_avoid[j,t-1] + 1 - y[j,t-1];
      n_shock[j,t] <- n_shock[j,t-1] + y[j,t-1];
      if (y[j,t-1] == 0) {
        n_trials_since_shock[j,t] <- n_trials_since_shock[j,t-1] + 1;
      }
      else {
        n_trials_since_shock[j,t] <- 0;
      }
    }
    for (t in 1:n_trials)
      p[j,t] <- beta[j, 1] + beta[j, 2] * n_avoid[j,t] + beta[j, 3] * n_shock[j,t] + beta[j, 4] * n_trials_since_shock[j,t];
  }
}
model {
  alpha ~ normal(0, 100);
  for (i in 1:n_dogs) {
    beta[i] ~ normal(alpha, tau);
    for (j in 1:n_trials)
      y[i,j] ~ bernoulli_logit(p[i,j]);
  }
}