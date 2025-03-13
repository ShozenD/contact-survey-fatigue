# Load libraries
library(yaml)
library(readr)
library(purrr)
library(data.table)
library(cmdstanr)
library(loo)
library(posterior)
library(bayesplot)
library(ggplot2)
library(devtools)
load_all()

theme_set(theme_bw())

# Load configurations
config <- read_yaml("config/varselect.yaml")
out_dir <- file.path(config$out_dir, "results", config$experiment_name)
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Load data
df <- read_rds(file.path("data", "silver", config$data$fname))

cat(" ===== Running stage 1 selection =====")
s1_results <- varselect_s1_stepwise(df, config)
write_rds(s1_results, file.path(out_dir, "s1_results.rds"))

cat(" ===== Running stage 2 analysis =====")
s2_results <- varselect_s2_stepwise(df, config, s1_results)
write_rds(s2_results, file.path(out_dir, "s2_results.rds"))

dt_loo_elpd <- s2_results$dt_loo_elpd
dt_loo_elpd$idx <- 1:nrow(dt_loo_elpd)
ggplot(dt_loo_elpd, aes(idx, loo_elpd)) +
  geom_line()
