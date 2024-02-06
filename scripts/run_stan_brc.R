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
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))

# ===== Load data =====
cat(" Loading data...\n")
data <- read_rds(file.path("data/silver", config$data$fname))

# ===== Prepare stan data =====
stan_data <- data
stan_data$Xhsgp <- scale(make_nn_lowertri_idxset(85))
stan_data$sym_from_lowertri_idxset <- make_sym_from_lowertri_idx(85)
stan_data$S <- make_S_matrix(config$model$M1, config$model$M2)

stan_data$M1 <- config$model$M1
stan_data$M2 <- config$model$M2
stan_data$C1 <- config$model$C1
stan_data$C2 <- config$model$C2

stan_data$hatGamma <- config$model$hatGamma
stan_data$hatKappa <- config$model$hatKappa
stan_data$hatEta <- config$model$hatEta

# Save stan_data for convienient access from different scripts
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "stan_data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
saveRDS(stan_data, file = file.path(out_dir, paste0(config$experiment_name, ".rds")))

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
                              refresh = 50)

# ===== Save fitted model =====
cat(" Saving the fitted model...\n")
# Setup output directory
out_dir <- file.path(config$out_dir, "stan_fits")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
stan_fit$save_object(file.path(out_dir, paste0(config$experiment_name, ".rds")))

cat(" Done!\n")
