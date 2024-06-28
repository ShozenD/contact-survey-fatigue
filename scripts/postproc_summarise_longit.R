# ===== Load libraries =====
library(yaml)
library(optparse)
library(readr)
library(stringr)
library(data.table)
library(reshape2)
library(cmdstanr)
library(posterior)
library(ggplot2)

# ===== Load configurations =====
option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading data and configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))
stan_data <- read_rds(file.path(config$out_dir, "stan_data", paste0(config$experiment_name, ".rds")))

cat(" Loading the fitted model...\n")
fit <- read_rds(file.path(config$out_dir, "stan_fits", paste0(config$experiment_name, ".rds")))

cat(" Summarizing posterior samples...\n")
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "results", config$experiment_name)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

quantile5 <- function(x) quantile(x, probs = c(.025, .25, .5, .75, .975))

# Extract posterior samples of rho (the repeat effect)
if (!str_detect(config$model$name, "_noadj")) { # If the model adjust for the repeat effect
  df_po_rho <- summarise_draws(fit$draws(variables = "rho"), ~quantile5(.x))
  df_po_rho$r <- seq(0, nrow(df_po_rho) - 1)
  saveRDS(df_po_rho, file.path(out_dir, "post_rho.rds"))

  # Extract posterior samples of gamma, kappa, and eta (the repeat effects)
  if (str_detect(config$model$name, "logistic")){
    df_po_rep_parms <- summarise_draws(fit$draws(variables = c("gamma", "zeta", "eta")), ~quantile5(.x))
    saveRDS(df_po_rep_parms, file.path(out_dir, "post_hill_parms.rds"))
  }
}

# Extract posterior samples of tau (time effect)
po <- fit$draws(variables = c("alpha", "tau", "beta[5]"), format = "matrix")
tmp <- sweep(po[,"alpha"], 1, po[,"beta[5]"], "+")
po <- sweep(po[, str_detect(colnames(po), "tau")], 1, tmp, "+")
df_po_tau <- summarise_draws(po, ~quantile5(exp(.x)))
saveRDS(df_po_tau, file.path(out_dir, "post_tau.rds"))

# Extract fixed effects
po_beta <- fit$draws(variables = c("beta"), format = "draws_matrix")
po_beta_sum <- summarise_draws(po_beta, ~quantile5(.x))
saveRDS(po_beta_sum, file.path(out_dir, "po_beta.rds"))

cat(" DONE!\n")