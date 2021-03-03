/*
 * weibull.stan
 * fit tree diameter distribution to Weibull distriution
 */

data {
  int<lower = 0> N;       // number of trees
  vector<lower = 0>[N] D; // tree diameters
}

parameters {
  real<lower = 0> alpha;  // shape parameter
  real<lower = 0> sigma;  // scale parameter
}

model {
  D ~ weibull(alpha, sigma);
  alpha ~ normal(0, 10);
  sigma ~ normal(0, 40);
}
