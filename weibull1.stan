data {
  int<lower = 0> N;       // number of trees
  vector<lower = 0>[N] D; // tree diameter data
}

parameters {
  real<lower = 0> alpha; // shape parameter
  real<lower = 0> sigma; // scale parameter
}

model {
  D ~ weibull(alpha, sigma);
}

generated quantities {
  vector<lower = 0>[N] yrep;

  for (n in 1:N)
    yrep[n] = weibull_rng(alpha, sigma);
}
