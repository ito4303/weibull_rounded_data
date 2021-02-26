library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

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