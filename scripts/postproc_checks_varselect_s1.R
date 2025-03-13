library(yaml)
library(optparse)
library(readr)
library(data.table)
library(rjson)
library(reshape2)
library(cmdstanr)
library(devtools)
library(stringr)
load_all()

option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading data and configurations...\n")
cli_args$config_file <- "varselect.yaml"
config <- read_yaml(file.path("config", cli_args$config_file))

cat(" Loading the fitted models...\n")
dir_fits <- file.path(config$out_dir, "stan_fits", config$experiment_name, "stage_1")
fit_m1 <- read_rds(file.path(dir_fits, "model_1.rds"))
fit_m2 <- read_rds(file.path(dir_fits, "model_2.rds"))

cat(" Computing model diagnositic statistics...\n")
out_dir_stats <- file.path(config$out_dir, "results", config$experiment_name, "stage_1")
out_dir_m1 <- file.path(out_dir_stats, "model_1")
out_dir_m2 <- file.path(out_dir_stats, "model_2")

if (!dir.exists(out_dir_stats)) {
  dir.create(out_dir_m1, recursive = TRUE)
  dir.create(out_dir_m2)
}

diag_stat_m1 <- make_convergence_diagnostic_stats(fit_m1, outdir = out_dir_m1)
diag_stat_m2 <- make_convergence_diagnostic_stats(fit_m2, outdir = out_dir_m2)

cat(" Performing posterior predictive checks...\n")
dir_data <- file.path(config$out_dir, "stan_data", config$experiment_name, "stage_1")
stan_data_m1 <- read_rds(file.path(dir_data, "model_1.rds"))
stan_data_m2 <- read_rds(file.path(dir_data, "model_2.rds"))

type_m1 <- unlist(str_split(config$stage_1$model_1$name, "_"))[1]
type_m2 <- unlist(str_split(config$stage_1$model_2$name, "_"))[1]
dt_ppc_m1 <- posterior_predictive_checks(fit_m1, stan_data, type_m1, out_dir_m1)
dt_ppc_m2 <- posterior_predictive_checks(fit_m2, stan_data, type_m2, out_dir_m2)

cat(" DONE!\n")
