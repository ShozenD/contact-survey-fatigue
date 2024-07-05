# ===== Load libraries =====
library(yaml)
library(optparse)
library(readr)
library(stringr)
library(data.table)
library(cmdstanr)
library(posterior)
library(devtools)
load_all()

# ===== Load configurations =====
option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file"),
  make_option(c("--arr_idx"), type = "integer", default = NA, help = "pbs array index", dest = "arr_idx")
)
cli_args <- parse_args(OptionParser(option_list = option_list))
REPEAT <- cli_args$arr_idx - 1

cat(" Loading data and configurations...\n")
config <- read_yaml(file.path("config", cli_args$config_file))
fname <- paste("covimod_wave", config$data$wave, "increp", REPEAT, sep = "_")
stan_data <- read_rds(file.path(config$out_dir, "stan_data", paste0(fname, ".rds")))

cat(" Loading the fitted model...\n")
fname <- paste(config$experiment_name, "increp", REPEAT, sep = "_")
fit <- read_rds(file.path(config$out_dir, "stan_fits", paste0(fname, ".rds")))

cat(" Computing model diagnositic statistics...\n")
out_dir <- config$out_dir
out_dir <- file.path(out_dir, "results", fname)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

fit_summary <- make_convergence_diagnostic_stats(fit, outdir = out_dir)

cat(" Summarizing posterior samples...\n")
colnames(stan_data$X)
# Weight by age and gender
w <- setDT(read_rds("data/population_weights/gender_by_age.rds"))
wm <- w[gender == "Male"]$weight
wf <- w[gender == "Female"]$weight

draws_log_m <- fit$draws("log_m", format = "matrix")
draws_beta_f <- fit$draws("beta[1]", format = "matrix")
draws_log_m_f <- sweep(draws_log_m, 1, draws_beta_f, "+")
draws_log_m_f <- sweep(draws_log_m_f, 2, log(wf), "+")
draws_log_m_m <- sweep(draws_log_m, 2, log(wm), "+")
draws_log_m <- log(exp(draws_log_m_f) + exp(draws_log_m_m))

# Weight by household size
w <- setDT(read_rds("data/population_weights/hhsize.rds"))
draws_log_m_1 <- sweep(draws_log_m, 1, fit$draws("beta[2]", format = "matrix"), "+") + log(w$weight[1])
draws_log_m_2 <- sweep(draws_log_m, 1, fit$draws("beta[3]", format = "matrix"), "+") + log(w$weight[2])
draws_log_m_3 <- draws_log_m + log(w$weight[3])
draws_log_m_4 <- sweep(draws_log_m, 1, fit$draws("beta[4]", format = "matrix"), "+") + log(w$weight[4])
draws_log_m_5 <- sweep(draws_log_m, 1, fit$draws("beta[5]", format = "matrix"), "+") + log(w$weight[5])
draws_log_m <- log(exp(draws_log_m_1) + exp(draws_log_m_2) + exp(draws_log_m_3) + exp(draws_log_m_4) + exp(draws_log_m_5))

# Marginal contact intensity
dt_mcint <- setDT(summarise_draws(draws_log_m, ~ quantile2(exp(.x), probs = c(0.025, 0.5, 0.975))))
dt_mcint[, age := as.numeric(gsub("log_m\\[([0-9]+)\\]", "\\1", variable)) - 1]
saveRDS(dt_mcint, file.path(out_dir, "marginal_contact_intensity.rds"))

# Extract fixed effects
df_beta <- fit$summary("beta", quantiles = ~ quantile2(., probs = c(0.025, 0.5, 0.975)))
saveRDS(df_beta, file.path(out_dir, "po_sum_beta.rds"))

cat(" Done!\n")
