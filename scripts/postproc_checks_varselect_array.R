library(yaml)
library(optparse)
library(readr)
library(data.table)
library(rjson)
library(reshape2)
library(cmdstanr)
library(devtools)
load_all()

option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file"),
  make_option(c("--arr_idx"), type = "integer", default = NA, help = "pbs array index", dest = "arr_idx")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading data and configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))

cat(" Loading the fitted model...\n")
fit <- read_rds(
  file.path(
    config$out_dir,
    "stan_fits",
    paste0(config$experiment_name, "_", as.character(cli_args$arr_idx), ".rds")
  )
)

cat(" Computing model diagnositic statistics...\n")
out_dir <- config$out_dir
out_dir <- file.path(
  out_dir,
  "results",
  paste0(config$experiment_name, "_", as.character(cli_args$arr_idx))
)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fit_summary <- make_convergence_diagnostic_stats(fit, outdir = out_dir)

cat(" Performing posterior predictive checks...\n")
stan_data <- read_rds(file.path(config$out_dir, "stan_data", paste0(config$experiment_name, ".rds")))
dt_ppc <- posterior_predictive_checks(fit, stan_data, config, out_dir)

cat(" Done!\n")