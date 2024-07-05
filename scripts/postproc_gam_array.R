# ===== Load libraries =====
library(yaml)
library(optparse)
library(readr)
library(stringr)
library(data.table)
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
fname <- paste("covimod_wave", config$data$wave, "increp", REPEAT, sep = "_")
stan_data <- read_rds(file.path(config$out_dir, "stan_data", paste0(fname, ".rds")))

cat(" Loading the fitted model...\n")
fname <- paste(config$experiment_name, "increp", REPEAT, sep = "_")
fit <- read_rds(file.path(config$out_dir, "stan_fits", paste0(fname, ".rds")))

cat(" Computing model diagnositic statistics...\n")
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "results", fname)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

fit_summary <- make_convergence_diagnostic_stats(fit, outdir = out_dir)

cat(" Summarizing posterior samples...\n")
# Contact intensity
po_cint <- fit$draws(variables = c("log_m"), format = "draws_matrix")
cint_sum <- setDT(posterior::summarize_draws(po_cint, exp_quantile5))
colnames(cint_sum) <- c("variable", "CL", "Q25", "M", "Q75", "CU")
cint_sum[, age := as.numeric(gsub("log_m\\[([0-9]+)\\]", "\\1", variable)) - 1]
saveRDS(cint_sum, file.path(out_dir, "cint_sum.rds"))

# Extract fixed effects
po_beta <- fit$draws(variables = c("beta"), format = "draws_matrix")
po_beta_sum <- summarise_draws(po_beta, ~quantile5(.x))
saveRDS(po_beta_sum, file.path(out_dir, "po_beta_sum.rds"))

cat(" Done!\n")
