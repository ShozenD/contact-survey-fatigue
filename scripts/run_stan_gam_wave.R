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
for (w in 1:33) {
  cat(" Wave ", w, "\n")

  # Load data
  fname <- paste0(paste(config$experiment_name, w, sep = "_"), ".rds")
  stan_data <- read_rds(file.path("data/silver", fname))

  # Load the fitted model for the previous wave
  if (w == 1) {
    stan_data$hatBeta <- rep(0, stan_data$P)
    stan_data$hatGamma <- rep(config$model$hatGamma, stan_data$Q)
    stan_data$hatZeta <- rep(config$model$hatZeta, stan_data$Q)
    stan_data$hatEta <- rep(config$model$hatEta, stan_data$Q)
  } else {
    fname <- paste0(paste(config$experiment_name, w - 1, sep = "_"), ".rds")
    fit <- read_rds(file.path(config$out_dir, "stan_fits", fname))
    stan_data$hatBeta <- fit$summary("beta", "mean")$mean
    stan_data$hatGamma <- fit$summary("gamma", "mean")$mean
    stan_data$hatZeta <- fit$summary("zeta", "mean")$mean
    stan_data$hatEta <- fit$summary("eta", "mean")$mean
  }

  # Save stan_data for convenient access from different scripts
  out_dir <- file.path(config$out_dir, "stan_data")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  fname <- paste(config$experiment_name, w, sep = "_")
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
                                refresh = 500)

  # ===== Save fitted model =====
  cat(" Saving the fitted model...\n")
  # Setup output directory
  out_dir <- file.path(config$out_dir, "stan_fits")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  fname <- paste(config$experiment_name, w, sep = "_")
  stan_fit$save_object(file.path(out_dir, paste0(fname, ".rds")))
}

cat(" DONE!\n")
