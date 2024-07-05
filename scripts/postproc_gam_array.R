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
# Marginal contact intensity
dt_mcint <- setDT(fit$summary("log_m", quantiles = ~ quantile2(exp(.), probs = c(0.025, 0.5, 0.975))))
dt_mcint[, age := as.numeric(gsub("log_m\\[([0-9]+)\\]", "\\1", variable)) - 1]
saveRDS(dt_mcint, file.path(out_dir, "marginal_contact_intensity.rds"))

# Extract fixed effects
df_beta <- fit$summary("beta", quantiles = ~ quantile2(., probs = c(0.025, 0.5, 0.975)))
saveRDS(df_beta, file.path(out_dir, "po_sum_beta.rds"))

cat(" Done!\n")
