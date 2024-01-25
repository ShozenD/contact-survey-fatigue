library(readr)
library(data.table)
library(cmdstanr)

data <- read_rds("data/silver/covimod-wave-20.rds")

stan_data <- data
stan_data$Xhsgp <- scale(make_nn_lowertri_idxset(85))
stan_data$sym_from_lowertri_idxset <- make_sym_from_lowertri_idx(85)
stan_data$S <- make_S_matrix(30, 30)

stan_data$M1 <- 30
stan_data$M2 <- 30
stan_data$C1 <- 1.5
stan_data$C2 <- 1.5

stan_data$hatGamma <- 1
stan_data$hatKappa <- 1
stan_data$hatEta <- 1

names(stan_data)

model <- cmdstan_model("stan_models/brc_se.stan", compile = TRUE)

fit <- model$sample(stan_data,
                    iter_warmup = 200,
                    iter_sampling = 100,
                    chains = 2,
                    parallel_chains = 2,
                    refresh = 10)
