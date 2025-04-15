# Import libraries
library(optparse)
library(yaml)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(cmdstanr)
library(posterior)
library(devtools)
load_all()

# ===== Load configurations =====
# Parse command line arguments
option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))

# Load the fitted model
cat(" Loading the fitted models...\n")
stan_data_dir <- file.path(config$out_dir, "stan_data")
stan_fit_paths <- list.files(path = file.path(config$out_dir, "stan_fits"),
                             pattern = config$experiment_name,
                             full.names = TRUE)

for (f in stan_fit_paths) {
  cat(" Fitted model file: ", f, "\n")
  stan_data <- read_rds(file.path(stan_data_dir, basename(f)))
  fit <- read_rds(f)

  cat(" Calculating quantities of interest...\n")
  dt_cint <- summarise_wcint(fit, stan_data)

  # Save the results
  out_dir <- file.path(config$out_dir, "results", str_remove(basename(f), ".rds$"))
  if (!dir.exists(out_dir)) dir.create(out_dir)
  write_rds(dt_cint, file.path(out_dir, "weighted_intensity.rds"))
}

cat(" DONE!\n")
