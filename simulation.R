#
# Fit rounded tree diameter data to Weibull model
#

library(cmdstanr)
options(mc.cores = parallel::detectCores())
library(posterior)

# Seed of RNG
set.seed(1)

# Number of replications
R <- 1000

# Function to generate rounded data
round_n <- function(x, n) {
  if (n != round(n))
    stop("n must be an integer number.")
  max_B <- floor(max(x) / n) * n + n
  boundary <- seq(0, max_B, n)
  n_classes <- length(boundary) - 1
  y <- rep(0, n_classes)
  for (i in seq_along(x)) {
    c <- floor(x[i] / n) + 1
    y[c] = y[c] + 1
  }
  list(y = y, 
       n_classes = n_classes,
       boundary = boundary[-1])
}

# Function for the simulation
sim <- function(N = 100, alpha = 2, sigma = 10) {
  est <- rep(NA, 8)

  # Initial values of MCMC
  inits <- list(list(alpha = 0.1, sigma = 18),
                list(alpha = 1.0, sigma = 12),
                list(alpha = 2.0, sigma = 10),
                list(alpha = 2.5, sigma = 6))

  # original data
  D <- rweibull(N, alpha, sigma) # tree diameter data (cm)
  data1 <- list(N = N, D = D)
  model1 <- cmdstan_model("weibull.stan")
  fit1 <- model1$sample(data = data1, init = inits,
                        refresh = 0, show_messages = FALSE,
                        chains = 4,
                        iter_sampling = 2000, iter_warmup = 2000)
  m <- fit1$summary(c("alpha", "sigma"), mean, rhat)
  if (max(m$rhat) > 1.1)
    stop("rhat > 1.1; ", m$rhat)
  est[1:2] <- m$mean

  # rounding to 0.1 cm
  D2 <- round(D, 1)
  data2 <- list(N = N, D = D2)
  fit2 <- model1$sample(data = data2, init = inits,
                        refresh = 0, show_messages = FALSE,
                        chains = 4,
                        iter_sampling = 2000, iter_warmup = 2000)
  m <- fit2$summary(c("alpha", "sigma"), mean, rhat)
  if (max(m$rhat) > 1.1)
    stop("rhat > 1.1; ", m$rhat)
  est[3:4] <- m$mean

  # rounding to 2 cm
  d3 <- round_n(D, 2)
  data3 <- list(K = d3$n_classes, B = d3$boundary, D = d3$y)
  model2 <- cmdstan_model("weibull_r.stan")
  fit3 <- model2$sample(data = data3, init = inits,
                        refresh = 0, show_messages = FALSE,
                        chains = 4,
                        iter_sampling = 2000, iter_warmup = 2000,
                        adapt_delta = 0.9)
  m <- fit3$summary(c("alpha", "sigma"), mean, rhat)
  if (max(m$rhat) > 1.1)
    stop("rhat > 1.1; ", m$rhat)
  est[5:6] <- m$mean

  # rounding to 5 cm
  d4 <- round_n(D, 5)
  data4 <- list(K = d4$n_classes, B = d4$boundary, D = d4$y)
  fit4 <- model2$sample(data = data4, init = inits,
                        refresh = 0, show_messages = FALSE,
                        chains = 4,
                        iter_sampling = 2000, iter_warmup = 2000,
                        adapt_delta = 0.95)
  m <- fit4$summary(c("alpha", "sigma"), mean, rhat)
  if (max(m$rhat) > 1.1)
    stop("rhat > 1.1; ", m$rhat)
  est[7:8] <- m$mean

  return(est)
}

# Simulation 1
# N = 30, alpha = 2, sigma = 8
print("simulation 1")
sim1 <- replicate(R, sim(N = 30, alpha = 2, sigma = 8))
apply(sim1, 1, mean)
apply(sim1, 1, sd)

# Simulation 2
# N = 300, alpha = 2, sigma = 8
print("simulation 2")
sim2 <- replicate(R, sim(N = 300, alpha = 2, sigma = 8))
apply(sim2, 1, mean)
apply(sim2, 1, sd)

# Simulation 3
# N = 30, alpha = 4, sigma = 15
print("simulation 3")
sim3 <- replicate(R, sim(N = 30, alpha = 4, sigma = 15))
apply(sim3, 1, mean)
apply(sim3, 1, sd)

# Simulation 4
# N = 300, alpha = 4, sigma = 15
print("simulation 4")
sim4 <- replicate(R, sim(N = 300, alpha = 4, sigma = 15))
apply(sim4, 1, mean)
apply(sim4, 1, sd)

