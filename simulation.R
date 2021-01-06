#
# A simulation to fit rounded tree diameter data to Weibull model
#

library(cmdstanr)
options(mc.cores = parallel::detectCores())
library(posterior)

# Set seed of RNG
set.seed(314)

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
sim_fun <- function(seed = 1, N = 100, alpha = 2, sigma = 10) {
  est <- rep(NA, 8)

  # Initial values of MCMC
  inits <- list(list(alpha = 0.1, sigma = 4),
                list(alpha = 1, sigma = 8),
                list(alpha = 2, sigma = 12),
                list(alpha = 3, sigma = 16))

  # original data
  D <- rweibull(N, alpha, sigma) # tree diameter data (cm)
  data1 <- list(N = N, D = D)
  model1 <- cmdstan_model("weibull.stan")
  fit1 <- model1$sample(data = data1, seed = seed, init = inits,
                        refresh = 0, show_messages = FALSE,
                        chains = 4,
                        iter_sampling = 2000, iter_warmup = 2000)
  m <- try(fit1$summary(c("alpha", "sigma"), mean, rhat))
  if (class(m)[1] == "draws_summary") {
    if (max(m$rhat) > 1.1)
      stop("rhat > 1.1; ", m$rhat)
    est[1:2] <- m$mean
  }
  
  # rounding to 0.1 cm
  D2 <- round(D, 1)
  data2 <- list(N = N, D = D2)
  fit2 <- model1$sample(data = data2, seed = seed, init = inits,
                        refresh = 0, show_messages = FALSE,
                        chains = 4,
                        iter_sampling = 2000, iter_warmup = 2000)
  m <- try(fit2$summary(c("alpha", "sigma"), mean, rhat))
  if (class(m)[1] == "draws_summary") {
    if (max(m$rhat) > 1.1)
      stop("rhat > 1.1; ", m$rhat)
    est[3:4] <- m$mean
  }

  # rounding to 2 cm
  d3 <- round_n(D, 2)
  data3 <- list(K = d3$n_classes, B = d3$boundary, D = d3$y)
  model2 <- cmdstan_model("weibull_r.stan")
  fit3 <- model2$sample(data = data3, seed = seed, init = inits,
                        refresh = 0, show_messages = FALSE,
                        chains = 4,
                        iter_sampling = 2000, iter_warmup = 2000,
                        adapt_delta = 0.9)
  m <- try(fit3$summary(c("alpha", "sigma"), mean, rhat))
  if (class(m)[1] == "draws_summary") {
    if (max(m$rhat) > 1.1)
      stop("rhat > 1.1; ", m$rhat)
    est[5:6] <- m$mean
  }
  
    # rounding to 5 cm
  d4 <- round_n(D, 5)
  data4 <- list(K = d4$n_classes, B = d4$boundary, D = d4$y)
  fit4 <- model2$sample(data = data4, seed = seed, init = inits,
                        refresh = 0, show_messages = FALSE,
                        chains = 4,
                        iter_sampling = 2000, iter_warmup = 2000,
                        adapt_delta = 0.98)
  m <- try(fit4$summary(c("alpha", "sigma"), mean, rhat))
  if (class(m)[1] == "draws_summary") {
    if (max(m$rhat) > 1.1)
      stop("rhat > 1.1; ", m$rhat)
    est[7:8] <- m$mean
  }

  return(est)
}


sim <- vector("list", 6)

# Simulation 1
# N = 30, alpha = 2, sigma = 8
sim[[1]] <- sapply(1:R,
                   function(i)
                     sim_fun(i, N = 30, alpha = 2, sigma = 8))
apply(!is.na(sim[[1]]), 1, sum)
apply(sim[[1]], 1, mean, na.rm = TRUE)
apply(sim[[1]], 1, sd, na.rm = TRUE)

# Simulation 2
# N = 300, alpha = 2, sigma = 8
sim[[2]] <- sapply(1:R,
                   function(i)
                     sim_fun(i, N = 300, alpha = 2, sigma = 8))
apply(!is.na(sim[[2]]), 1, sum)
apply(sim[[2]], 1, mean, na.rm = TRUE)
apply(sim[[2]], 1, sd, na.rm = TRUE)

# Simulation 3
# N = 30, alpha = 4, sigma = 12
sim[[3]] <- sapply(1:R,
                   function(i)
                     sim_fun(i, N = 30, alpha = 4, sigma = 12))
apply(!is.na(sim[[3]]), 1, sum)
apply(sim[[3]], 1, mean, na.rm = TRUE)
apply(sim[[3]], 1, sd, na.rm = TRUE)

# Simulation 4
# N = 300, alpha = 4, sigma = 12
sim[[4]] <- sapply(1:R,
                   function(i)
                     sim_fun(i, N = 300, alpha = 4, sigma = 12))
apply(!is.na(sim[[4]]), 1, sum)
apply(sim[[4]], 1, mean, na.rm = TRUE)
apply(sim[[4]], 1, sd, na.rm = TRUE)

# Simulation 5
# N = 30, alpha = 5, sigma = 20
sim[[5]] <- sapply(1:R,
                   function(i)
                     sim_fun(i, N = 30, alpha = 5, sigma = 20))
apply(!is.na(sim[[5]]), 1, sum)
apply(sim[[5]], 1, mean, na.rm = TRUE)
apply(sim[[5]], 1, sd, na.rm = TRUE)

# Simulation 6
# N = 300, alpha = 5, sigma = 20
sim[[6]] <- sapply(1:R,
                   function(i)
                     sim_fun(i, N = 300, alpha = 5, sigma = 20))
apply(!is.na(sim[[6]]), 1, sum)
apply(sim[[6]], 1, mean, na.rm = TRUE)
apply(sim[[6]], 1, sd, na.rm = TRUE)

save(sim, file = "sim_results.RData")