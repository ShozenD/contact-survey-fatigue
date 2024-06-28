# ===== Load libraries =====
library(yaml)
library(optparse)
library(readr)
library(stringr)
library(data.table)
library(reshape2)
library(cmdstanr)
library(posterior)
library(devtools)

load_all()

# ===== Load configurations =====
option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file"),
  make_option(c("--arr_idx"), type = "integer", default = NA, help = "pbs array index", dest = "arr_idx")
)
cli_args <- parse_args(OptionParser(option_list = option_list))
REPEAT <- cli_args$arr_idx - 1

cat(" Loading data and configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))
fname <- paste("covimod_wave_21", "increp", REPEAT, sep = "_")
stan_data <- read_rds(file.path(config$out_dir, "stan_data", paste0(fname, ".rds")))

cat(" Loading the fitted model...\n")
fname <- paste(config$experiment_name, "increp", REPEAT, sep = "_")
fit <- read_rds(file.path(config$out_dir, "stan_fits", paste0(fname, ".rds")))

cat(" Summarizing posterior samples...\n")
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "results", fname)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

quantile5 <- function(x) quantile(x, probs = c(.025, .25, .5, .75, .975))

# Extract contact intensity
po <- fit$draws(variables = c("alpha", "log_m"), format = "draws_matrix")
cint_matrix <- extract_cint_matrix(po)

# Contact intensity matrix
cint_matrix_sum <- summarise_cint_matrix(cint_matrix)
saveRDS(cint_matrix_sum, file.path(out_dir, "cint_matrix_sum.rds"))

# Marginal contact intensity
cint_margin_sum <- summarise_cint_marginal(cint_matrix)
saveRDS(cint_margin_sum, file.path(out_dir, "cint_margin_sum.rds"))

# Extract fixed effects
po_beta <- fit$draws(variables = c("beta"), format = "draws_matrix")
po_beta_sum <- summarise_draws(po_beta, ~quantile5(.x))
saveRDS(po_beta_sum, file.path(out_dir, "po_beta_sum.rds"))

cat(" Done!\n")