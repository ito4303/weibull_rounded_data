#
# Fit rounded tree diameter data to Weibull model
#

library(ggplot2)
library(cmdstanr)
set_cmdstan_path("~/.cmdstan/cmdstan-2.25.0")
options(mc.cores = parallel::detectCores())

# generate simulated data
set.seed(123)

N <- 100     # sample size
alpha <- 2.  # shape parameter of Weibull distribution
sigma <- 10  # scale parameter of Weibull distribution

D <- rweibull(N, alpha, sigma) # tree diameter data (cm)

ggplot(data.frame(Diameter = D)) +
  geom_histogram(aes(x = Diameter), binwidth = 1, boundary = 0) +
  labs(x = "Diameter (cm)", y = "Frequency")

ggplot(data.frame(Diameter = D)) +
  geom_histogram(aes(x = Diameter), binwidth = 2, boundary = 0) +
  labs(x = "Diameter (cm)", y = "Frequency")

ggplot(data.frame(Diameter = D)) +
  geom_histogram(aes(x = Diameter), binwidth = 5, boundary = 0) +
  labs(x = "Diameter (cm)", y = "Frequency")

# fit to Weibull distribution
data1 <- list(N = N, D = D)
model1 <- cmdstan_model("weibull1.stan")
fit1 <- model1$sample(data = data1, refresh = 1000,
                      chains = 4,
                      iter_sampling = 1000, iter_warmup = 1000)
fit1$summary()

# rounding to 0.1 cm
D2 <- round(D, 1)
data2 <- list(N = N, D = D2)
# fit to the same model
fit2 <- model1$sample(data = data2, refresh = 1000,
                      chains = 4,
                      iter_sampling = 1000, iter_warmup = 1000)
fit2$summary()


# rounding to 2 cm
round_n <- function(x, n) {
  if (n != round(n))
    stop("n must be an integer number.")
  max_B <- floor(max(D) / n) * n + n
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

d3 <- round_n(D, 2)
data3 <- list(K = d3$n_classes, B = d3$boundary, D = d3$y)
model2 <- cmdstan_model("weibull2.stan")
fit3 <- model2$sample(data = data3, refresh = 1000,
                      chains = 4,
                      iter_sampling = 1000, iter_warmup = 1000)
fit3$summary()

# rounding to 5 cm
d4 <- round_n(D, 5)
data4 <- list(K = d4$n_classes, B = d4$boundary, D = d4$y)
fit4 <- model2$sample(data = data4, refresh = 1000,
                      chains = 4,
                      iter_sampling = 1000, iter_warmup = 1000)
fit4$summary()


