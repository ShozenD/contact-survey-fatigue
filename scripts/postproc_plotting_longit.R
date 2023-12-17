# ===== Load libraries =====
library(yaml)
library(optparse)
library(readr)
library(data.table)
library(reshape2)
library(cmdstanr)
library(ggplot2)

# ===== Load configurations =====
option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cli_args$config_file <- "pois_longit_gp.yaml"

cat(" Loading data and configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))
stan_data <- read_rds(file.path(config$out_dir, "stan_data", paste0(config$model$name, ".rds")))

cat(" Loading the fitted model...\n")
fit <- read_rds(file.path(config$out_dir, "stan_fits", paste0(config$model$name, ".rds")))

# Extract posterior samples of rho (the repeat effect)
post_samples_rho <- fit$draws(variables = c("rho"), format = "matrix")

# Compute the quantiles of rho
quantile5 <- function(x) quantile(x, probs = c(0.025, .25, 0.5, .75, 0.975))
post_samples_rho_sum <- apply(post_samples_rho, 2, quantile5)
post_samples_rho_sum <- t(post_samples_sum)
colnames(post_samples_rho_sum) <- c("CL", "Q25", "M", "Q75", "CU")
dt_post_samples_rho <- data.table(post_samples_rho_sum)
dt_post_samples_rho[, rep := seq(0, nrow(dt_post_samples_rho) - 1)]

ggplot(dt_post_samples_rho, aes(x = rep, y = M)) +
  geom_line() +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.2) +
  geom_ribbon(aes(ymin = CL, ymax = CU), alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Repetition", y = "rho") +
  theme_bw()

# Extract posterior samples of tau (time effect
post_samples_tau <- fit$draws(variables = c("tau"), format = "matrix")

post_samples_tau_sum <- apply(post_samples_tau, 2, quantile5)
post_samples_tau_sum <- t(post_samples_tau_sum)
colnames(post_samples_tau_sum) <- c("CL", "Q25", "M", "Q75", "CU")
dt_post_samples_tau <- data.table(post_samples_tau_sum)
dt_post_samples_tau[, time := seq(1, nrow(dt_post_samples_tau))]

ggplot(dt_post_samples_tau, aes(x = time, y = M)) +
  geom_line() +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.2) +
  geom_ribbon(aes(ymin = CL, ymax = CU), alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Time", y = "tau") +
  theme_bw()
