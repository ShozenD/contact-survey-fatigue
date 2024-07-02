library(readr)
library(data.table)
library(cmdstanr)
library(posterior)

# ===== Load configurations =====
# Parse command line arguments
option_list <- list(
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file"),
  make_option(c("--arr_idx"), type = "integer", default = NA, help = "pbs array index", dest = "arr_idx")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

# Load population data
pop_data <- setDT(read_csv("data/germany-population-2011.csv"))

# Load the fitted model
fit_dir <- file.path(config$out_dir, "stan_fits")
fname <- paste(config$experiment_name, WAVE, sep = "_")
fit <- read_rds(file.path(fit_dir, paste0(fname, ".rds")))

# Extract posterior draws
draws_log_m <- fit$draws("log_m", format = "matrix")
draws_beta <- fit$draws("beta", format = "matrix")

# Helper function to calculate age and gender weighted intensity
age_gender_weighted_intensity <- function(age_range, draws_log_m, draws_beta, pop, lab) {
  p <- pop[age %in% age_range]
  p[, w := pop/sum(pop)]
  x1 <- sweep(draws_log_m[,(age_range + 1)], 1, draws_beta[,1], "+")
  x1 <- sweep(exp(x1), 2, p[gender == "Male"]$w, "*")
  x1 <- as.numeric(rowSums(x1))

  x2 <- sweep(draws_log_m[,(age_range + 1)], 1, draws_beta[,2], "+")
  x2 <- sweep(exp(x2), 2, p[gender == "Female"]$w, "*")
  x2 <- as.numeric(rowSums(x2))

  dt <- as.data.table(t(quantile2(x1 + x2, c(0.025, 0.5, 0.975))))
  dt$lab <- lab

  return(dt)
}

# Calculate the contact intensity for different age brackets
dt_cnt_int <- rbind(
  age_gender_weighted_intensity(0:84, draws_log_m, draws_beta, pop_data, "all"),
  age_gender_weighted_intensity(0:4, draws_log_m, draws_beta, pop_data, "0-4"),
  age_gender_weighted_intensity(5:17, draws_log_m, draws_beta, pop_data, "5-17"),
  age_gender_weighted_intensity(18:59, draws_log_m, draws_beta, pop_data, "18-59"),
  age_gender_weighted_intensity(60:84, draws_log_m, draws_beta, pop_data, "60-84")
)

# Save the results
out_dir <- file.path(config$out_dir, "results", paste(config$experiment_name, WAVE, sep = "_"))
if (!dir.exists(out_dir)) dir.create(out_dir)
write_rds(dt_cnt_int, file.path(out_dir, "weighted_intensity.rds"))


