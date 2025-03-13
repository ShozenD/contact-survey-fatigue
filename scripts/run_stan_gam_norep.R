# Import libraries
library(yaml)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(cmdstanr)
library(posterior)
library(devtools)
load_all()

# ========== Load data ==========
cat(" Loading data and configurations...\n")
covimod_data <- read_rds("./data/COVIMOD/COVIMOD_data_2022-12-29.rds")
nuts <- read_rds(file.path("data", "nuts_info.rds"))
config <- read_yaml("config/negb_gam_array.yaml")

# Unpack data
dt_part <- data.table(covimod_data$part)
dt_hh <- data.table(covimod_data$hh)
dt_nhh <- data.table(covimod_data$nhh)

# ========== Data preprecoessing ==========
# Count the number of previous participations for each participant
setkeyv(dt_part, cols = c("new_id", "wave"))
dt_part[, rep := seq_len(.N) - 1, by = .(new_id)]

# Count the number of participants by wave and repeat
tbl <- dt_part[, .(N = .N), by = .(wave, rep)]
incl_wave <- sort(tbl[wave != 1 & rep == 0 & N > 300, wave]) # Waves with more than 300 participants with 0 repeats

# ========== Data preprecoessing ==========
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

build_stan_data <- function(w) {
  dt_cnt <- suppressWarnings(preproc_gam_data(w, dt_part, dt_hh, dt_nhh, nuts))
  dt_cnt <- dt_cnt[rep == 0]

  # ===== Make dummy variables =====
  dum_sex <- make_dummy_matrix(dt_cnt, "gender")[,"Female"]                      # Gender
  dum_hhsize <- make_dummy_matrix(dt_cnt, "hh_size", remove_first_dummy = TRUE)  # Household size
  dum_job <- make_dummy_matrix(dt_cnt, "job", c("full_time", "self_employed", "student", "long_term_sick", # Job
                                                "unemployed_looking", "unemployed_not_looking", "full_time_parent"))
  dum_urbn <- make_dummy_matrix(dt_cnt, "urbn_type", c("intermediate", "urban")) # Urban type

  if (length(unique(dt_cnt$dow)) == 1) {
    X <- cbind(dum_sex, dum_hhsize, dum_job, dum_urbn)
  } else {
    dum_dow <- make_dummy_matrix(dt_cnt, "dow")[,"weekend"]       # Day of week
    X <- cbind(dum_sex, dum_hhsize, dum_dow, dum_job, dum_urbn)
  }

  # ===== Prepare indexes =====
  aid <- dt_cnt$imp_age + 1

  # ===== HSGP =====
  x_hsgp <- seq(0, 84)
  x_hsgp <- (x_hsgp - mean(x_hsgp)) / sd(x_hsgp)

  # ===== Gather and export the data =====
  list(
    N = nrow(dt_cnt),
    A = 85,
    P = ncol(X),
    X = X,
    aid = aid,
    M = config$model$M,
    C = config$model$C,
    x_hsgp = x_hsgp,
    y = dt_cnt$y
  )
}

# Make Stan data
stan_data_list <- purrr::map(incl_wave, ~build_stan_data(.x))

stan_model <- cmdstan_model("stan_models/negb_gam_noadj.stan")
result_list <- purrr::map(stan_data_list, ~{
  fit_vb <- stan_model$variational(.x, show_messages = FALSE)
  fit <- stan_model$sample(.x,
                           iter_warmup = 5e2,
                           iter_sampling = 2e3,
                           chains = 4,
                           parallel_chains = 4,
                           max_treedepth = 10,
                           adapt_delta = 0.92,
                           init = fit_vb,
                           refresh = 0,
                           show_messages = FALSE,
                           show_exceptions = FALSE)

  # Extract posterior draws
  draws_log_m <- fit$draws("log_m", format = "matrix")
  draws_beta <- fit$draws("beta", format = "matrix")

  # Calculate the contact intensity for different age brackets
  rbind(
    weighted_intensity(0:84, draws_log_m, draws_beta, "all"),
    weighted_intensity(0:4, draws_log_m, draws_beta, "0-4"),
    weighted_intensity(5:17, draws_log_m, draws_beta, "5-17"),
    weighted_intensity(18:59, draws_log_m, draws_beta, "18-59"),
    weighted_intensity(60:84, draws_log_m, draws_beta, "60-84")
  )
})

dt_results <- purrr::map_dfr(1:length(result_list), ~{
  d <- result_list[[.x]]
  d$wave <- incl_wave[.x]
  d
})

# Save the results
write_rds(dt_results, "../contact-survey-fatigue-outputs/results/model_noadj_wave.rds")
