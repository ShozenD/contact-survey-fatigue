# ===== Load libraries =====
library(yaml)
library(optparse)
library(readr)
library(stringr)
library(data.table)
library(reshape2)
library(cmdstanr)
library(posterior)
library(ggplot2)

# ===== Load configurations =====
option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

cat(" Loading data and configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))
stan_data <- read_rds(file.path(config$out_dir, "stan_data", paste0(config$experiment_name, ".rds")))

cat(" Loading the fitted model...\n")
fit <- read_rds(file.path(config$out_dir, "stan_fits", paste0(config$experiment_name, ".rds")))

cat(" Summarizing posterior samples...\n")
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "results", config$experiment_name)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Extract posterior samples of rho (the repeat effect)
if (!str_detect(config$model$name, "_noadj")) { # If the model adjust for the repeat effect
  df_rho <- fit$summary("rho",
                        posterior::default_summary_measures()[1:4],
                        quantiles = ~ quantile2(., probs = c(.025, .975)))
  if (!str_detect(config$model$name, "_single")) {
    df_rho$r <- seq(0, nrow(df_rho) - 1)
  }
  saveRDS(df_rho, file.path(out_dir, "po_sum_rho.rds"))

  # Extract posterior samples of gamma, zeta, and eta (the repeat effects)
  if (str_detect(config$model$name, "hill")){
    df_hill <- fit$summary(c("gamma", "zeta", "eta"),
                           posterior::default_summary_measures()[1:4],
                           quantiles = ~ quantile2(., probs = c(.025, .975)))
    saveRDS(df_hill, file.path(out_dir, "po_sum_hill.rds"))
  }
}

# Extract posterior samples of tau (time effect)
draws <- fit$draws(c("beta0", "tau", "beta_gender", "beta_age"), format = "matrix")

marginal_cint <- function(draws, variable, label) {
  x <- sweep(draws[, "beta0"], 1, draws[,"beta_gender[1]"], "+") # Calculate for males
  x <- sweep(x, 1, draws[,variable], "+")
  x <- sweep(draws[, str_detect(colnames(draws), "tau")], 1, x, "+")
  df <- summarise_draws(x, ~quantile2(exp(.x), probs = c(.025, .5, .975)))
  df$label <- label

  return(df)
}

df_mcint <- marginal_cint(draws, "beta_age[9]", "Male, 45-54")
df_mcint <- rbind(df_mcint, marginal_cint(draws, "beta_age[13]", "Male, 75-79"))
df_mcint <- rbind(df_mcint, marginal_cint(draws, "beta_age[3]", "Male, student [6-9]"))
saveRDS(df_mcint, file.path(out_dir, "marginal_contact_intensity.rds"))

# Extract fixed effects
vars <- fit$metadata()$stan_variables
df_beta <- fit$summary(vars[grep("beta_.*", vars)],
                       posterior::default_summary_measures()[1:4],
                       quantiles = ~ quantile2(., probs = c(.025, .975)))
saveRDS(df_beta, file.path(out_dir, "po_sum_beta.rds"))

cat(" DONE!\n")
