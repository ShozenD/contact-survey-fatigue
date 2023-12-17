# ===== Load libraries =====
library(yaml)
library(optparse)
library(readr)
library(dplyr)
library(cmdstanr)

# Source functions
source("R/make_stan_data.R")

# ===== Load configurations =====
# Parse command line arguments
option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))

# ===== Load data =====
cat(" Loading data...\n")
data <- read_rds("data/silver/covimod-wave-3to12.rds")

# ===== Prepare data =====
cat(" Preparing Stan data...\n")
stan_data <- make_stan_data(data, config)

# Save stan_data for convienient access from different scripts
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "stan_data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
saveRDS(stan_data, file = file.path(out_dir, paste0(config$model$name, ".rds")))

# ===== Compile Stan model =====
cat(" Compiling Stan model...\n")
stan_model <- cmdstan_model(file.path("stan_models", paste0(config$model$name, ".stan")),
                            compile = TRUE)

# ===== Run MCMC =====
cat(" Running MCMC...\n")
stan_fit <- stan_model$sample(data = stan_data,
                              seed = config$mcmc$seed,
                              chains = config$mcmc$chains,
                              parallel_chains = config$mcmc$parallel_chains,
                              iter_warmup = config$mcmc$iter_warmup,
                              iter_sampling = config$mcmc$iter_sampling,
                              max_treedepth = config$mcmc$max_treedepth,
                              refresh = 10)

# ===== Save fitted model =====
cat(" Saving the fitted model...\n")
# Setup output directory
out_dir <- file.path(config$out_dir, "stan_fits")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
stan_fit$save_object(file.path(out_dir, paste0(config$model$name, ".rds")))
