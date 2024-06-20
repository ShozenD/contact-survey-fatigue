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
config <- read_yaml(file.path("config", cli_args$config_file))
REPEAT <- cli_args$arr_idx - 1

# Unpack data
dt_part <- data.table(covimod_data$part)
dt_hh <- data.table(covimod_data$hh)
dt_nhh <- data.table(covimod_data$nhh)
dt_pop <- data.table(covimod_data$pop)

# ========== Data preprecoessing ==========
# Count the number of previous participations for each participant
setkeyv(dt_part, cols = c("new_id", "wave"))
dt_part[, rep := seq_len(.N) - 1, by = .(new_id)]

# Filter participants by their number of repeats
dt_part <- dt_part[rep <= REPEAT]

# Extract data from the specified wave
dt_part <- dt_part[wave == config$data$wave]
dt_hh <- dt_hh[wave == config$data$wave]
dt_nhh <- dt_nhh[wave == config$data$wave]

# Sort dt_part by new_id
dt_part <- dt_part[order(new_id)]

# Remove participants with missing values in the gender column
dt_part <- dt_part[!is.na(gender)]

# Remove participants with missing values in the age_strata column
dt_part <- dt_part[!is.na(age_strata)]

# Impute missing ages for children
dt_part <- fill_missing_child_ages(dt_part, seed = 123)

# ===== Prepare contact count vector Y =====
# For nhh Count the number of rows (contacts) by participant and alter_age_strata
dt_nhh_sum <- dt_nhh[, .(y_nhh = .N), by = new_id]

# For hh sum the number of rows (contacts) by participant and alter_age_strata
dt_hh_sum <- dt_hh[, .(y_hh = sum(hh_met_this_day)), by = new_id]

# In dt_part, sum the values in columns Q75_u18_work to Q75_o64_else and save it as y_grp
SDcols <- colnames(dt_part)[str_detect(colnames(dt_part), "Q")]
dt_part[, y_grp := rowSums(.SD, na.rm = TRUE), .SDcols = SDcols]
dt_grp <- dt_part[, .(new_id, y_grp)]

# Merge the three data.tables
dt_y <- merge(dt_grp, dt_hh_sum, by = "new_id", all.x = TRUE)
dt_y <- merge(dt_y, dt_nhh_sum, by = "new_id", all.x = TRUE)

# Replace missing values with 0
dt_y[is.na(y_grp), y_grp := 0]
dt_y[is.na(y_hh), y_hh := 0]
dt_y[is.na(y_nhh), y_nhh := 0]

# Sum the three columns to get the total number of contacts
dt_y[, y := y_grp + y_hh + y_nhh]

# ===== Prepare participant characteristics X =====
### Occupations
jobs_of_interest <- c("Full-time parent, homemaker",
                      "Long-term sick or disabled",
                      "Unemployed and not looking for a job",
                      "Unemployed but looking for a job")
dt_part[, job_2 := ifelse(job %in% jobs_of_interest, as.character(job), "Other")]
dt_dum <- fastDummies::dummy_cols(
  dt_part[, .(new_id, job_2)],
  select_columns = "job_2",
  remove_selected_columns = TRUE,
  omit_colname_prefix = TRUE
)
dt_dum[, Other := NULL] # Remove the Other column
dt_dum <- dt_dum[,-1]

# ===== Prepare repeat effects dummy =====
# Age variables
age_strata_of_interest <- c("0-4", "5-9", "10-14", "15-19", "20-24", "45-54", "75-79")
dt_part[, age_strata_2 := ifelse(age_strata %in% age_strata_of_interest, as.character(age_strata), "Other")]
dt_rep_dum_age <- fastDummies::dummy_cols(dt_part[, .(new_id, age_strata_2)],
                                          select_columns = "age_strata_2",
                                          remove_selected_columns = TRUE,
                                          omit_colname_prefix = TRUE)
dt_rep_dum_age[, Other := NULL] # Remove the Other column

# Jobs
jobs_of_interest <- c("Employed full-time (34 hours or more)",
                      "Employed part-time (less than 34 hours)",
                      "Self-employed",
                      "Unemployed and not looking for a job")
dt_part[, job_2 := ifelse(job %in% jobs_of_interest, as.character(job), "Other")]
dt_rep_dum_job <- fastDummies::dummy_cols(dt_part[, .(new_id, job_2)],
                                          select_columns = "job_2",
                                          remove_selected_columns = TRUE,
                                          omit_colname_prefix = TRUE)
dt_rep_dum_job[, Other := NULL] # Remove the Other column

dt_rep_dum <- merge(dt_rep_dum_age, dt_rep_dum_job)
rep_matrix <- as.matrix(dt_rep_dum[, -1])
jid <- rep(0, nrow(rep_matrix))
for (i in 1:nrow(rep_matrix)) {
  if (sum(rep_matrix[i,]) > 0) {
    jid[i] <- last(which(rep_matrix[i,] > 0, arr.ind = TRUE))
  }
}

rid <- dt_part$rep

# ===== Prepare indexes =====
# Age
aid <- dt_part$imp_age + 1
aid[aid > 85] <- 85

# ===== HSGP =====
a_min <- 0
a_max <- 84
x_hsgp <- seq(a_min, a_max)
x_hsgp <- (x_hsgp - mean(x_hsgp)) / sd(x_hsgp)

# ===== Gather and export the data =====
stan_data <- list(
  # Sample size and dimensions
  N = nrow(dt_y),
  A = max(aid),
  P = ncol(dt_dum),
  J = ncol(dt_rep_dum),
  R = max(dt_part$rep) + 1,

  # Outcome
  Y = dt_y$y,

  # Covariates
  X = as.matrix(dt_dum),

  # Indexes
  aid = aid,
  jid = jid,
  rid = rid,

  # Repeat effect prior
  hatGamma = config$model$hatGamma,
  hatKappa = config$model$hatKappa,
  hatEta = config$model$hatEta,

  # HSGP
  M = config$model$M,
  C = config$model$C,
  x_hsgp = x_hsgp
)

file_name <- paste("covimod-wave", config$data$wave, "increp", as.character(REPEAT), sep = "-")
file_name <- paste0(file_name, ".rds")
saveRDS(stan_data, file.path("data/silver", file_name))

cat(" DONE!\n")




