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
# Extract posterior draws
draws_log_m <- fit$draws("log_m", format = "matrix")
draws_beta <- fit$draws("beta", format = "matrix")

# Helper function to calculate age and gender weighted intensity
weighted_intensity <- function(age_range, draws_log_m, draws_beta, label) {
  # Load and process population weights by age and gender
  w <- setDT(read_rds("data/population_weights/age_and_gender.rds"))
  w <- w[age %in% age_range]
  w[, weight := weight / sum(weight)] # Renormalize weights

  # Separate weights by gender
  wf <- w[gender == "Female"]$weight
  wm <- w[gender == "Male"]$weight

  # Calculate log_m for females and males
  log_m <- draws_log_m[, age_range + 1]
  log_m_f <- sweep(log_m, 1, draws_beta[, 1], "+")
  log_m_f <- sweep(log_m_f, 2, log(wf), "+")
  log_m_m <- sweep(log_m, 2, log(wm), "+")
  log_m <- log(rowSums(exp(log_m_f) + exp(log_m_m)))

  # Load and process household size weights
  hh_weights <- setDT(read_rds("data/population_weights/hhsize.rds"))$weight

  # Calculate log_m for different household sizes
  log_m_list <- lapply(1:5, function(i) {
    if (i == 3) {
      return(log_m + log(hh_weights[i]))
    } else {
      return(log_m + draws_beta[, i + 1] + log(hh_weights[i]))
    }
  })

  # Sum the exponentiated log_m values
  m <- Reduce(`+`, lapply(log_m_list, exp))

  # Summarize the results
  dt <- setDT(summarise_draws(m, ~quantile2(.x, c(0.025, 0.5, 0.975))))
  dt[, variable := NULL]
  dt[, label := label]

  return(dt)
}

# Calculate the contact intensity for different age brackets
dt_cint <- rbind(
  weighted_intensity(0:84, draws_log_m, draws_beta, "all"),
  weighted_intensity(0:4, draws_log_m, draws_beta, "0-4"),
  weighted_intensity(5:17, draws_log_m, draws_beta, "5-17"),
  weighted_intensity(18:59, draws_log_m, draws_beta, "18-59"),
  weighted_intensity(60:84, draws_log_m, draws_beta, "60-84")
)

# Save the results
out_dir <- file.path(config$out_dir, "results", paste(config$experiment_name, WAVE, sep = "_"))
if (!dir.exists(out_dir)) dir.create(out_dir)
write_rds(dt_cint, file.path(out_dir, "weighted_intensity.rds"))

cat(" DONE!\n")
