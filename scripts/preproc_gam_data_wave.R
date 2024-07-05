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
  make_option(c("--config"), type = "character", default = NA, help = "configuration file", dest = "config_file"),
  make_option(c("--arr_idx"), type = "integer", default = NA, help = "pbs array index", dest = "arr_idx")
)
cli_args <- parse_args(OptionParser(option_list = option_list))

# ========== Load data ==========
cat(" Loading data and configurations...\n")
covimod_data <- read_rds("./data/COVIMOD/COVIMOD_data_2022-12-29.rds")
nuts <- read_rds(file.path("data", "nuts_info.rds"))

config <- read_yaml(file.path("config", cli_args$config_file))
WAVE <- cli_args$arr_idx

# Unpack data
dt_part <- data.table(covimod_data$part)
dt_hh <- data.table(covimod_data$hh)
dt_nhh <- data.table(covimod_data$nhh)

# ========== Data preprecoessing ==========
cat(" Preprocessing data...\n")
dt_cnt <- preproc_gam_data(WAVE, dt_part, dt_hh, dt_nhh, nuts)

# ===== Prepare participant characteristics X =====
make_dummy_matrix <- function(data, variable, include = NULL, ...) {
  data <- setDT(data)
  data <- data[, ..variable]
  data <- fastDummies::dummy_cols(data,
                                  select_columns = variable,
                                  remove_selected_columns = TRUE,
                                  omit_colname_prefix = TRUE,
                                  ...)
  if (!is.null(include)) data <- data[, ..include]
  data <- as.matrix(data)
  data[is.na(data)] <- 0

  return(data)
}

# ===== Make dummy variables =====
dum_sex <- make_dummy_matrix(dt_cnt, "gender")[,"Female"]                      # Gender
dum_hhsize <- make_dummy_matrix(dt_cnt, "hh_size", remove_first_dummy = TRUE)  # Household size
dum_job <- make_dummy_matrix(dt_cnt, "job", c("full_time", "self_employed", "student", "long_term_sick", # Job
                                              "unemployed_looking", "unemployed_not_looking", "full_time_parent"))
dum_urbn <- make_dummy_matrix(dt_cnt, "urbn_type", c("intermediate", "urban")) # Urban type

if (WAVE == 10) {
  X <- cbind(dum_sex, dum_hhsize, dum_job, dum_urbn)
} else {
  dum_dow <- make_dummy_matrix(dt_cnt, "dow")[,"weekend"]       # Day of week
  X <- cbind(dum_sex, dum_dow, dum_hhsize, dum_job, dum_urbn)
}

# ===== Prepare repeat effects dummy =====
include_age_strata <- unique(dt_cnt$age_strata)
include_age_strata <- include_age_strata[!(include_age_strata %in% c("25-34", "35-44"))]
rdum_age <- make_dummy_matrix(dt_cnt, "age_strata", include_age_strata)
rdum_sex <- make_dummy_matrix(dt_cnt, "gender", "Female")
rdum_hhsize <- make_dummy_matrix(dt_cnt, "hh_size", "1")
rdum_job <- make_dummy_matrix(dt_cnt, "job", c("full_time", "part_time", "self_employed", "student",
                                               "long_term_sick", "unemployed_looking", "unemployed_not_looking"))
rdum_urbn <- make_dummy_matrix(dt_cnt, "urbn_type", c("rural", "intermediate"))
Z <- cbind(rdum_age, rdum_sex, rdum_hhsize, rdum_job)

# ===== Prepare indexes =====
aid <- dt_cnt$imp_age + 1
rid <- dt_cnt$rep + 1

# ===== HSGP =====
x_hsgp <- seq(0, 84)
x_hsgp <- (x_hsgp - mean(x_hsgp)) / sd(x_hsgp)

# ===== Gather and export the data =====
stan_data <- list(
  N = nrow(dt_cnt),
  A = 85,
  P = ncol(X),
  Q = ncol(Z),

  X = X,
  Z = Z,
  aid = aid,
  rid = rid,

  hatGamma = config$model$hatGamma,
  hatZeta = config$model$hatZeta,
  hatEta = config$model$hatEta,

  M = config$model$M,
  C = config$model$C,
  x_hsgp = x_hsgp,

  y = dt_cnt$y
)

file_name <- paste(config$experiment_name, WAVE, sep = "_")
file_name <- paste0(file_name, ".rds")
saveRDS(stan_data, file.path("data/silver", file_name))

cat(" DONE!\n")



