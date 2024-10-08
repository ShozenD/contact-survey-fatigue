library(yaml)
library(optparse)
library(readr)
library(cmdstanr)
library(devtools)
load_all()

option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file"),
  make_option(c("--arr_idx"), type = "integer", default = NA, help = "pbs array index", dest = "arr_idx")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading data and configurations...\n")
df <- read_rds("data/silver/covimod-wave-4.rds")
config <- read_yaml(file.path("config", cli_args$config_file))

cat(" Making Stan data...\n")
stan_data <- make_stan_data_varselect(df, config)
stan_data$p0 <- cli_args$arr_idx

# Save stan_data for convienient access from different scripts
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "stan_data")
if (!dir.exists(out_dir)) dir.create(out_dir)
if (file.exists(file.path(out_dir, paste0(config$experiment_name, ".rds")))) {
  cat(" Stan data already exists!\n")
} else {
  saveRDS(stan_data, file = file.path(out_dir, paste0(config$experiment_name, ".rds")))
}

cat(" Compiling Stan model...\n")
stan_model <- cmdstan_model(file.path("stan_models", paste0(config$model$name, ".stan")),
                            compile = TRUE)

cat(" Running MCMC...\n")
stan_fit <- stan_model$sample(stan_data,
                              seed = config$mcmc$seed,
                              iter_warmup = config$mcmc$iter_warmup,
                              iter_sampling = config$mcmc$iter_sampling,
                              chains = config$mcmc$chains,
                              parallel_chains = config$mcmc$parallel_chains,
                              max_treedepth = config$mcmc$max_treedepth,
                              adapt_delta = config$mcmc$adapt_delta,
                              refresh = 100)

cat(" Saving the fitted model...\n")
# Setup output directory
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "stan_fits")
if (!dir.exists(out_dir)) dir.create(out_dir)
stan_fit$save_object(
  file.path(out_dir, paste0(config$experiment_name, "_" , as.character(cli_args$arr_idx), ".rds"))
)

cat(" Done!\n")
