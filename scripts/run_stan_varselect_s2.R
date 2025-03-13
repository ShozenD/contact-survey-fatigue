library(readr)
library(data.table)
library(cmdstanr)
library(devtools)
load_all()

# ===== Helpers =====
fit_model_s2 <- function(stan_data) {
  model <- cmdstan_model("stan_models/negb_varselect_s2.stan", compile = TRUE)
  fit <- model$sample(data = stan_data,
                      chains = 4,
                      parallel_chains = 4,
                      iter_warmup = 500,
                      iter_sampling = 1000,
                      max_treedepth = 13,
                      adapt_delta = 0.99)
  return(fit)
}

# ===== Start =====
# Load the data
df.w4 <- read_rds("data/silver/covimod_wave_4.rds")
df.w21 <- read_rds("data/silver/covimod_wave_21.rds")

# Load results from stage 1
su.s1 <- read_rds("../contact-survey-fatigue-outputs/results/varselect/summary_stage_1.rds")
fit.s1.w4 <- read_rds("../contact-survey-fatigue-outputs/stan_fits/varselect/fit_s1_w4.rds")
fit.s1.w21 <- read_rds("../contact-survey-fatigue-outputs/stan_fits/varselect/fit_s1_w21.rds")
stan_data.w4 <- read_rds("../contact-survey-fatigue-outputs/stan_data/varselect/stan_data_s1_w4.rds")
stan_data.w21 <- read_rds("../contact-survey-fatigue-outputs/stan_data/varselect/stan_data_s1_w21.rds")

# Make Stan data for stage 2
stan_data.w4 <- make_stan_data_varselect_s2(df.w4, fit.s1.w4, su.s1)
stan_data.w21 <- make_stan_data_varselect_s2(df.w21, fit.s1.w21, su.s1)

# Fit stage 2 model
fit.w4 <- fit_model_s2(stan_data.w4)
fit.w21 <- fit_model_s2(stan_data.w21)

# Summarise posterior draws
su.s2.w4 <- fit.w4$summary("gamma", quantiles = ~ quantile2(., probs = c(0.25, 0.5, 0.75))) |> setDT()
su.s2.w21 <- fit.w21$summary("gamma", quantiles = ~ quantile2(., probs = c(0.25, 0.5, 0.75))) |> setDT()

# Add variable names
su.s2.w4$varname <- colnames(stan_data.w4$W)
su.s2.w21$varname <- colnames(stan_data.w21$W)

# Clean labels
su.s2.w4 <- clean_labels_s2(su.s2.w4, stan_data.w4)
su.s2.w21 <- clean_labels_s2(su.s2.w21, stan_data.w21)

# Add wave
su.s2.w4$wave <- "Wave 4"
su.s2.w21$wave <- "Wave 21"

# Combine
su.s2 <- rbind(su.s2.w4, su.s2.w21)
su.s2$wave <- factor(su.s2$wave, levels = c("Wave 4", "Wave 21"))
su.s2$x <- as.numeric(su.s2$category)
su.s2[, selected := q50 < log(0.95)]

selected_vars <- sort(unique(su.s2[selected == TRUE, varname]))

if (!dir.exists("../contact-survey-fatigue-outputs/results/varselect")) {
  dir.create("../contact-survey-fatigue-outputs/results/varselect", recursive = TRUE)
}
write_rds(su.s2, "../contact-survey-fatigue-outputs/results/varselect/summary_stage_2.rds")
