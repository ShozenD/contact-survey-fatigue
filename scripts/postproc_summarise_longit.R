# ===== Load libraries =====
library(yaml)
library(optparse)
library(readr)
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

cli_args$config_file <- "pois_longit_gp.yaml"

cat(" Loading data and configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))
stan_data <- read_rds(file.path(config$out_dir, "stan_data", paste0(config$model$name, ".rds")))

cat(" Loading the fitted model...\n")
fit <- read_rds(file.path(config$out_dir, "stan_fits", paste0(config$model$name, ".rds")))

cat(" Summarizing posterior samples...\n")
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "results", config$model$name)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

quantile5 <- function(x) quantile(x, probs = c(.025, .25, .5, .75, .975))

# Extract posterior samples of rho (the repeat effect)
df_po_rho <- summarise_draws(fit$draws(variables = "rho"), ~quantile5(.x))
df_po_rho$r <- seq(0, nrow(df_po_rho) - 1)
saveRDS(df_po_rho, file.path(out_dir, "post_rho.rds"))

# Extract posterior samples of tau (time effect)
po <- fit$draws(variables = c("alpha", "tau"), format = "matrix")
po <- sweep(po[, -1], 1, po[, 1], "+")
df_po_tau <- summarise_draws(po, ~quantile5(exp(.x)))
saveRDS(df_po_tau, file.path(out_dir, "post_tau.rds"))
