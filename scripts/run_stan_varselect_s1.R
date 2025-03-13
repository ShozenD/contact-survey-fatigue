library(readr)
library(purrr)
library(data.table)
library(cmdstanr)
library(posterior)
library(devtools)
library(patchwork)
load_all()

# ===== Helpers =====
fit_model_s1 <- function(standata) {
  model <- cmdstan_model("stan_models/negb_varselect_s1.stan", compile=TRUE)
  fit <- model$sample(data = standata,
                      chains = 4,
                      parallel_chains = 4,
                      iter_warmup = 500,
                      iter_sampling = 1000,
                      max_treedepth = 13,
                      adapt_delta = 0.99,
                      show_exceptions = FALSE,
                      refresh = 100)
  return(fit)
}

# ===== Start ======

# Load the data
df.w4 <- read_rds("data/silver/covimod_wave_4.rds")
df.w21 <- read_rds("data/silver/covimod_wave_21.rds")

# Prepare the data
stan_data.w4 <- make_stan_data_varselect_s1(df.w4)
stan_data.w21 <- make_stan_data_varselect_s1(df.w21)

if (!dir.exists("../contact-survey-fatigue-outputs/stan_data/varselect")) {
  dir.create("../contact-survey-fatigue-outputs/stan_data/varselect", recursive = TRUE)
}
write_rds(stan_data.w4, "../contact-survey-fatigue-outputs/stan_data/varselect/stan_data_s1_w4.rds")
write_rds(stan_data.w21, "../contact-survey-fatigue-outputs/stan_data/varselect/stan_data_s1_w21.rds")

# Fit the model
fit.s1.w4 <- fit_model_s1(stan_data.w4)   # Wave 4
write_rds(fit.s1.w4, "../contact-survey-fatigue-outputs/stan_fits/varselect/fit_s1_w4.rds")

fit.s1.w21 <- fit_model_s1(stan_data.w21) # Wave 21
write_rds(fit.s1.w21, "../contact-survey-fatigue-outputs/stan_fits/varselect/fit_s1_w21.rds")

vars <- fit.s1.w4$metadata()$stan_variables
su.s1.w4 <- fit.s1.w4$summary(vars[grep("(alpha|beta)_.*", vars)],
                              quantiles = ~ quantile2(., probs = c(0.025, 0.5, 0.975))) |> setDT()
su.s1.w21 <- fit.s1.w21$summary(vars[grep("(alpha|beta)_.*", vars)],
                                quantiles = ~ quantile2(., probs = c(0.025, 0.5, 0.975))) |> setDT()

# Clean the labels
su.s1.w4 <- clean_labels(su.s1.w4, stan_data.w4)
su.s1.w21 <- clean_labels(su.s1.w21, stan_data.w21)

threshold <- 0.05 # The percentage increase or decrease
su.s1.w4[, selected := !between(q50, log(1 - threshold), log(1 + threshold))]
su.s1.w21[, selected := !between(q50, log(1 - threshold), log(1 + threshold))]

su.s1.w4[, wave := "Wave 4"]
su.s1.w21[, wave := "Wave 21"]
su.s1 <- rbind(su.s1.w4, su.s1.w21)
su.s1$wave <- factor(su.s1$wave, levels = c("Wave 4", "Wave 21"))
su.s1$x <- as.numeric(su.s1$category)

if (!dir.exists("../contact-survey-fatigue-outputs/results/varselect")) {
  dir.create("../contact-survey-fatigue-outputs/results/varselect", recursive = TRUE)
}
write_rds(su.s1, "../contact-survey-fatigue-outputs/results/varselect/summary_stage_1.rds")
