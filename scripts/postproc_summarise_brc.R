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

# Extract contact intensity
po <- fit$draws(variables = c("alpha", "log_m"), format = "draws_matrix")
cint_matrix <- extract_cint_matrix(po)

# Contact intensity matrix
cint_matrix_sum <- summarise_cint_matrix(cint_matrix)
saveRDS(cint_matrix_sum, file.path(out_dir, "cint_matrix_sum.rds"))

# Marginal contact intensity
cint_margin_sum <- summarise_cint_marginal(cint_matrix)
saveRDS(cint_margin_sum, file.path(out_dir, "cint_margin_sum.rds"))

cat(" Done!\n")