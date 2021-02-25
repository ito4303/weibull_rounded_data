library(dplyr)
library(tibble)
library(tidyr)

load("sim_results.RData")

n_sim <- 6

n <- sapply(seq_len(n_sim), function(i)
  apply(!is.na(sim[[i]]), 1, sum))
m <- sapply(seq_len(n_sim), function(i)
  apply(sim[[i]], 1, mean, na.rm = TRUE))
s <- sapply(seq_len(n_sim), function(i)
  apply(sim[[i]], 1, sd, na.rm = TRUE))
data_type <- rep(1:4, each = 2)
results <- tibble(simulation = rep(seq_len(n_sim), each = 8),
                  data_type = rep(data_type, n_sim),
                  var = rep(c("alpha", "sigma"), n_sim * 4),
                  num = c(n),
                  mean = c(m),
                  sd = c(s))

tbl_alpha <- results %>%
  dplyr::filter(var == "alpha")
tbl_sigma <- results %>%
  dplyr::filter(var == "sigma")

tbl_alpha %>%
  tidyr::pivot_wider(names_from = data_type,
                     values_from = c(num, mean, sd)) %>%
  dplyr::select(starts_with("mean"))
tbl_alpha %>%
  tidyr::pivot_wider(names_from = data_type,
                     values_from = c(num, mean, sd)) %>%
  dplyr::select(starts_with("sd"))
tbl_sigma %>%
  tidyr::pivot_wider(names_from = data_type,
                     values_from = c(num, mean, sd)) %>%
  dplyr::select(starts_with("mean"))
tbl_sigma %>%
  tidyr::pivot_wider(names_from = data_type,
                     values_from = c(num, mean, sd)) %>%
  dplyr::select(starts_with("sd"))