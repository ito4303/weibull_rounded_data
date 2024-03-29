/*
 * weibull_r.stan
 * fit tree diameter class to Weibull distriution
 */

data {
  int<lower = 0> K;       // number of classes
  positive_ordered[K] B;  // boundary of classes
  int<lower = 0> D[K];    // number of trees for each class
}

transformed data {
  // append one 0 for the class larger than the B[K]
  array[K + 1] int D2 = append_array(D, {0});
}

parameters {
  real<lower = 0> alpha; // shape parameter
  real<lower = 0> sigma; // scale parameter
}

transformed parameters {
  simplex[K + 1] p;     // probs for each class

  p[1] = weibull_cdf(B[1] | alpha, sigma); // 0 -- B[1]
  for (k in 2:K)
    p[k] = weibull_cdf(B[k] | alpha, sigma)
           - weibull_cdf(B[k - 1] | alpha, sigma); // B[1] -- B[K]
  p[K + 1] = 1 - weibull_cdf(B[K] | alpha, sigma); // B[K] --
}

model {
  D2 ~ multinomial(p);
  alpha ~ normal(0, 10);
  sigma ~ normal(0, 40);
}
