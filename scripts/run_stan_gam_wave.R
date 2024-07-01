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
WAVE <- cli_args$arr_idx

# ===== Load data =====
cat(" Loading data...\n")
fname <- paste("covimod_wave", WAVE, "gam", sep = "_")
fname <- paste0(fname, ".rds")
stan_data <- read_rds(file.path("data/silver", fname))

# Save stan_data for convienient access from different scripts
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "stan_data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fname <- paste("covimod_wave_", WAVE, "gam", sep = "_")
saveRDS(stan_data, file = file.path(out_dir, paste0(fname, ".rds")))

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
                              refresh = 100)

# ===== Save fitted model =====
cat(" Saving the fitted model...\n")
# Setup output directory
out_dir <- file.path(config$out_dir, "stan_fits")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fname <- paste("covimod_wave_", WAVE, "gam", sep = "_")
stan_fit$save_object(file.path(out_dir, paste0(fname, ".rds")))

cat(" DONE!\n")
