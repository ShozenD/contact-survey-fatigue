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
files <- list.files(path = file.path("data", "silver"),
                    pattern = config$experiment_name,
                    full.names = TRUE)
for (f in files) {
  cat(" Data file: ", f, "\n")

  # Load data
  stan_data <- read_rds(f)

  # Fixed effects
  stan_data$hat_beta_sex <- rep(0, stan_data$P_sex)
  stan_data$hat_beta_hhsize <- rep(0, stan_data$P_hhsize)
  stan_data$hat_beta_job <- rep(0, stan_data$P_job)
  stan_data$hat_beta_urbn <- rep(0, stan_data$P_urbn)

  # Save stan_data for convenient access from different scripts
  out_dir <- file.path(config$out_dir, "stan_data")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  saveRDS(stan_data, file = file.path(out_dir, basename(f)))

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
  stan_fit$save_object(file.path(out_dir, basename(f)))
}

cat(" DONE!\n")
