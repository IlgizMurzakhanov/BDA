data {
  int<lower = 0> N;
  int<lower = 1, upper = 2> city[N];
  int<lower = 1, upper = 4> grade[N];
  vector[N] electric_pretest;
  vector[N] electric_posttest;
  vector[N] control_pretest;
  vector[N] control_posttest;
}
parameters {
  real tv_effect;
  real beta_city[2];
  real beta_grade[4];
  real<lower = 0> sigma;
}
model {
  for (n in 1:N) {
    electric_posttest ~ normal(electric_pretest + tv_effect + beta_city[city[n]] + beta_grade[grade[n]], sigma[1]);
    control_posttest ~ normal(control_pretest + beta_city[city[n]] + beta_grade[grade[n]], sigma[2]);
  }
}