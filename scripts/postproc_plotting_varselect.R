library(yaml)
library(optparse)
library(readr)
library(data.table)
library(reshape2)
library(cmdstanr)

source("R/plot_repeat_effects.R")
source("R/plot_baseline_effects.R")

option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading data and configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))
stan_data <- read_rds(file.path(config$out_dir, "stan_data", paste0(config$model$name, ".rds")))

cat(" Loading the fitted model...\n")
fit <- read_rds(file.path(config$out_dir, "stan_fits", paste0(config$model$name, ".rds")))

out_dir <- config$out_dir
out_dir <- file.path(out_dir, "results", config$model$name, "figures")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cat( "Plotting baseline effects...\n")
plt_baseeff <- plot_baseline_effects(fit, stan_data, config, out_dir)

cat( "Plotting repeat effects...\n")
plt_repeff <- plot_repeat_effects(fit, stan_data, config, out_dir)
