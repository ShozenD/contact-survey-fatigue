library(yaml)
library(optparse)
library(readr)
library(data.table)
library(cmdstanr)
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

# ===== Load data =====
cat(" Loading data...\n")

# Load data
repeat_cap <- config$data$repeat_cap
min_ssize <- config$data$min_ssize
file_list <- list.files("data/silver", pattern = paste(config$experiment_name, repeat_cap, min_ssize, sep = "_"))

if (cli_args$arr_idx <= length(file_list)) { # If the array index is within the range
  fname <- file_list[cli_args$arr_idx]
  stan_data <- read_rds(file.path("data/silver", fname))

  # Save stan_data for convenient access from different scripts
  out_dir <- file.path(config$out_dir, "stan_data")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  saveRDS(stan_data, file = file.path(out_dir, fname))

  # ===== Compile Stan model =====
  cat(" Compiling Stan model...\n")
  stan_model <- cmdstan_model(file.path("stan_models", paste0(config$model$name, ".stan")),
                              compile = TRUE)

  # ===== Run MCMC =====
  cat(" Running MCMC...\n")
  stan_fit <- stan_model$sample(stan_data,
                                iter_warmup = config$mcmc$iter_warmup,
                                iter_sampling = config$mcmc$iter_sampling,
                                chains = config$mcmc$chains,
                                parallel_chains = config$mcmc$parallel_chains,
                                max_treedepth = config$mcmc$max_treedepth,
                                adapt_delta = config$mcmc$adapt_delta,
                                refresh = 500)

  # ===== Save fitted model =====
  cat(" Saving the fitted model...\n")
  # Setup output directory
  out_dir <- file.path(config$out_dir, "stan_fits")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  stan_fit$save_object(file.path(out_dir, fname))
} else {
  cat(" No data file found for the array index ", cli_args$arr_idx, "\n")
  cat(" Skipping...\n")
}

cat(" DONE!\n")
