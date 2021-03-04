library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(ggplot2)

load("sim_results.RData")

n_sim <- 6
n_type <- 4

n <- sapply(seq_len(n_sim), function(i)
  apply(!is.na(sim[[i]]), 1, sum))
m <- sapply(seq_len(n_sim), function(i)
  apply(sim[[i]], 1, mean, na.rm = TRUE))
s <- sapply(seq_len(n_sim), function(i)
  apply(sim[[i]], 1, sd, na.rm = TRUE))
data_type <- rep(seq_len(n_type), each = 2)
results <- tibble(sim = rep(seq_len(n_sim), each = n_type * 2),
                  type = rep(data_type, n_sim),
                  var = rep(c("alpha", "sigma"), n_sim * n_type),
                  num = c(n),
                  mean = c(m),
                  sd = c(s))

# function to summarize the results
result_tbl <- function(results, var_name = "alpha") {
  results %>%
    dplyr::filter(var == var_name) %>%
    dplyr::transmute(
      sim, type,
      est = str_c(sprintf("%2.1f", mean),
                  "±",
                  sprintf("%2.1f", sd))) %>%
    tidyr::pivot_wider(names_from = type,
                       names_prefix = "data_type",
                       values_from = est)
}

# alpha
alpha_tbl <- results %>%
  result_tbl("alpha")
print(alpha_tbl)
write.csv(alpha_tbl, "simulation_alpha.csv")

# sigma
sigma_tbl <-  results %>%
  result_tbl("sigma")
print(sigma_tbl)
write.csv(sigma_tbl, "simulation_sigma.csv")

# n
results %>%
  dplyr::select(sim, type, var, num) %>%
  dplyr::filter(var == "alpha") %>%
  tidyr::pivot_wider(names_from = type,
                     values_from = num) %>%
  dplyr::select(-var)

# differences comparing the corresponding results with double precision
n_iter <- ncol(sim[[1]])
diff_alpha <- diff_sigma <- structure(rep(NA, n_sim * (n_type - 1) * n_iter),
                                      dim = c(n_sim, n_type - 1, n_iter))
for (s in seq_len(n_sim)) {
  for (t in seq_len(n_type - 1)) {
    diff_alpha[s, t, ] <- sim[[s]][t * 2 + 1, ] - sim[[s]][1, ]
    diff_sigma[s, t, ] <- sim[[s]][t * 2 + 2, ] - sim[[s]][2, ]
  }
}

diff_tbl <- tibble(sim = rep(seq_len(n_sim), (n_type - 1) * n_iter),
                   type = rep(rep(2:n_type, each = n_sim), n_iter),
                   diff_alpha = c(diff_alpha),
                   diff_sigma = c(diff_sigma))

# mean
diff_tbl %>%
  dplyr::group_by(sim, type) %>%
  dplyr::summarize(alpha = mean(diff_alpha, na.rm = TRUE),
                   sigma = mean(diff_sigma, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = type,
                     values_from = c(alpha, sigma))

# plot
diff_tbl %>%
  dplyr::filter(!is.na(diff_alpha)) %>%
ggplot() +
  geom_histogram(aes(x = diff_alpha), binwidth = 0.1) +
  facet_grid(sim ~ type)

diff_tbl %>%
  dplyr::filter(!is.na(diff_sigma)) %>%
ggplot() +
  geom_histogram(aes(x = diff_sigma), binwidth = 0.1) +
  facet_grid(sim ~ type)
