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
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file"),
  make_option(c("--arr_idx"), type = "integer", default = NA, help = "pbs array index", dest = "arr_idx")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))
WAVE <- cli_args$arr_idx

# Load the fitted model
cat(" Loading the fitted model...\n")
fit_dir <- file.path(config$out_dir, "stan_fits")
fname <- paste(config$experiment_name, WAVE, sep = "_")
fit <- read_rds(file.path(fit_dir, paste0(fname, ".rds")))

cat(" Calculating quantities of interest...\n")
dt_cint <- summarise_wcint(fit)

# Save the results
out_dir <- file.path(config$out_dir, "results", paste(config$experiment_name, WAVE, sep = "_"))
if (!dir.exists(out_dir)) dir.create(out_dir)
write_rds(dt_cint, file.path(out_dir, "weighted_intensity.rds"))

cat(" DONE!\n")
