# Import libraries
library(optparse)
library(yaml)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(devtools)
load_all()

# ========== Parse command line arguments ==========
option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

# ========== Load data ==========
cat(" Loading data and configurations...\n")
covimod_data <- read_rds("./data/COVIMOD/COVIMOD_data_2022-12-29.rds")
nuts <- read_rds(file.path("data", "nuts_info.rds"))

config <- read_yaml(file.path("config", cli_args$config_file))

# Unpack data
dt_part <- data.table(covimod_data$part)
dt_hh <- data.table(covimod_data$hh)
dt_nhh <- data.table(covimod_data$nhh)

# ========== Data preprecoessing ==========
cat(" Preprocessing data...\n")
for (w in 1:33) {
  cat(paste0("  Wave ", w, "...\n"))
  dt_cnt <- preproc_gam_data(w, dt_part, dt_hh, dt_nhh, nuts)
  n_first_time <- nrow(dt_cnt[rep == 0])

  if (n_first_time < 300) {
    cat("  Not enough data from first time respondents. Skipping...\n")
    next
  } else {
    cat("  Enough data from first time respondents.\n")
    first_time_idx <- which(dt_cnt$rep == 0)  # Find indices where rep equals 0

    # ===== Make dummy variables =====
    X_sex <- make_dummy_matrix(dt_cnt, "gender", "Female")[first_time_idx, , drop = FALSE] # Reference: Male
    X_hhsize <- make_dummy_matrix(dt_cnt, "hh_size", c("1", "2", "4", "5+"))[first_time_idx,] # Reference: 3
    X_job <- make_dummy_matrix(dt_cnt, "job", remove_first_dummy = TRUE)[first_time_idx,] # Reference: full_time
    X_urbn <- make_dummy_matrix(dt_cnt, "urbn_type", c("intermediate"))[first_time_idx, , drop = FALSE] # Reference: urban

    # ===== Prepare indexes =====
    aid <- dt_cnt$imp_age + 1
    aid <- aid[first_time_idx]

    # ===== HSGP =====
    x_hsgp <- seq(0, 84)
    x_hsgp <- (x_hsgp - mean(x_hsgp)) / sd(x_hsgp)

    # ===== Gather and export the data =====
    stan_data <- list(
      N = length(first_time_idx),
      A = 85,
      P_sex = ncol(X_sex),
      P_hhsize = ncol(X_hhsize),
      P_job = ncol(X_job),
      P_urbn = ncol(X_urbn),

      X_sex = X_sex,
      X_hhsize = X_hhsize,
      X_job = X_job,
      X_urbn = X_urbn,
      aid = aid,

      M = 30,
      C = 1.5,
      x_hsgp = x_hsgp,

      y = dt_cnt$y[first_time_idx]
    )

    file_name <- paste(config$experiment_name, w, sep = "_")
    file_name <- paste0(file_name, ".rds")
    saveRDS(stan_data, file.path("data/silver", file_name))
  }
}

cat(" DONE!\n")



