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
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))

# ===== Load data =====
cat(" Loading data...\n")
fname <- paste("covimod_wave", config$data$wave, "increp", REPEAT, sep = "_")
fname <- paste0(fname, ".rds")
stan_data <- read_rds(file.path("data/silver", fname))

# Save stan_data for convienient access from different scripts
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "stan_data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fname <- paste(config$experiment_name, "increp", REPEAT, sep = "_")
saveRDS(stan_data, file = file.path(out_dir, paste0(fname, ".rds")))

# ===== Compile Stan model =====
cat(" Compiling Stan model...\n")
stan_model <- cmdstan_model(file.path("stan_models", paste0(config$model$name, ".stan")),
                            compile = TRUE)

# Use variational bayes for a quick fit
init_func <- function() {
  list(
    alpha = rnorm(1, 1.33, 0.1),
    reciprocal_phi = rexp(1, 1),
    sigma_beta = rcauchy(1, 0, 1)
  )
}

fit_vb <- stan_model$variational(data = stan_data, seed = 0, init = init_func)

# ===== Run MCMC =====
cat(" Running MCMC...\n")
stan_fit <- stan_model$sample(stan_data,
                              seed = 0,
                              iter_warmup = config$mcmc$iter_warmup,
                              iter_sampling = config$mcmc$iter_sampling,
                              chains = config$mcmc$chains,
                              parallel_chains = config$mcmc$parallel_chains,
                              max_treedepth = config$mcmc$max_treedepth,
                              adapt_delta = config$mcmc$adapt_delta,
                              init = fit_vb)

# ===== Save fitted model =====
cat(" Saving the fitted model...\n")
# Setup output directory
out_dir <- file.path(config$out_dir, "stan_fits")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fname <- paste(config$experiment_name, "increp", REPEAT, sep = "_")
stan_fit$save_object(file.path(out_dir, paste0(fname, ".rds")))

cat(" Done!\n")
