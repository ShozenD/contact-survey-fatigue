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
dt_part <- dt_part[age_strata != "85+"]

# Impute missing ages for children
dt_part <- fill_missing_child_ages(dt_part, seed = 123)

# Preprocess age_strata and job variables
dt_part <- preproc_age_job(dt_part)

# Preprocess household size variable
dt_part[, hh_size := ifelse(hh_p_incl_0 >= 5, "5+", as.character(hh_p_incl_0))]
dt_part[, hh_p_incl_0 := NULL]

# Calculate day of week
dt_part[, dow := lubridate::wday(date, label = TRUE)]
dt_part[, dow := ifelse(dow %in% c("Sat", "Sun"), "weekend", "weekday")]

# Merge NUTS info
dt_nuts <- as.data.table(nuts)
dt_nuts <- dt_nuts[LEVL_CODE == 3 & CNTR_CODE == "DE", .(NUTS_NAME, URBN_TYPE)]
dt_part <- merge(dt_part, dt_nuts, by = "NUTS_NAME", all.x = TRUE)
dt_part[, urbn_type := case_when(URBN_TYPE == "1" ~ "urban",
                                 URBN_TYPE == "2" ~ "intermediate",
                                 URBN_TYPE == "3" ~ "rural")]
dt_part[, URBN_TYPE := NULL]

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
## Gender
gender_dum <- fastDummies::dummy_cols(dt_part[,.(gender)],
                                      select_columns = "gender",
                                      remove_selected_columns = TRUE,
                                      omit_colname_prefix = TRUE)
gender_dum <- as.matrix(gender_dum)

## Household size
hh_dum <- fastDummies::dummy_cols(
  dt_part[, .(hh_size)],
  select_columns = "hh_size",
  remove_selected_columns = TRUE,
  omit_colname_prefix = TRUE
)
hh_dum <- as.matrix(hh_dum)

## Occupations
include_jobs <- c("full_time", "self_employed", "student",
                  "long_term_sick", "unemployed_looking", "unemployed_not_looking",
                  "full_time_parent")
job_dum <- fastDummies::dummy_cols(
  dt_part[, .(job)],
  select_columns = "job",
  remove_selected_columns = TRUE,
  omit_colname_prefix = TRUE
)
job_dum <- as.matrix(job_dum[, ..include_jobs])
job_dum[is.na(job_dum)] <- 0

## Day of week
dow_dum <- fastDummies::dummy_cols(
  dt_part[, .(dow)],
  select_columns = "dow",
  remove_selected_columns = TRUE,
  omit_colname_prefix = TRUE
)
dow_dum <- as.matrix(dow_dum)

## Population density
include_urbn <- c("intermediate", "urban")
urbn_dum <- fastDummies::dummy_cols(
  dt_part[, .(urbn_type)],
  select_columns = "urbn_type",
  remove_selected_columns = TRUE,
  omit_colname_prefix = TRUE
)
urbn_dum <- urbn_dum[, ..include_urbn]
urbn_dum <- as.matrix(urbn_dum)

X <- cbind(gender_dum, hh_dum, job_dum, dow_dum, urbn_dum)

# ===== Prepare repeat effects dummy =====
## Age variables
include_age_strata <- unique(dt_part$age_strata)
include_age_strata <- include_age_strata[!(include_age_strata %in% c("25-34", "35-44"))]
rdum_age <- fastDummies::dummy_cols(dt_part[, .(age_strata)],
                                    select_columns = "age_strata",
                                    remove_selected_columns = TRUE,
                                    omit_colname_prefix = TRUE)
rdum_age <- as.matrix(rdum_age[, ..include_age_strata])

## Gender
include_gender <- c("Female")
rgender_dum <- fastDummies::dummy_cols(dt_part[, .(gender)],
                                       select_columns = "gender",
                                       remove_selected_columns = TRUE,
                                       omit_colname_prefix = TRUE)
rgender_dum <- as.matrix(rgender_dum[, ..include_gender])

## Household size
include_hhsize <- "1"
rhh_size_dum <- fastDummies::dummy_cols(dt_part[, .(hh_size)],
                                        select_columns = "hh_size",
                                        remove_selected_columns = TRUE,
                                        omit_colname_prefix = TRUE)
rhh_size_dum <- as.matrix(rhh_size_dum[, ..include_hhsize])

## Jobs
include_jobs <- c("full_time", "part_time", "self_employed", "student",
                  "long_term_sick", "unemployed_looking", "unemployed_not_looking")
rjob_dum <- fastDummies::dummy_cols(dt_part[, .(job)],
                                    select_columns = "job",
                                    remove_selected_columns = TRUE,
                                    omit_colname_prefix = TRUE)
rjob_dum <- as.matrix(rjob_dum[, ..include_jobs])
rjob_dum[is.na(rjob_dum)] <- 0

## Population density
include_urbn <- c("rural", "intermediate")
rurbn_dum <- fastDummies::dummy_cols(dt_part[, .(urbn_type)],
                                     select_columns = "urbn_type",
                                     remove_selected_columns = TRUE,
                                     omit_colname_prefix = TRUE)
rurbn_dum <- as.matrix(rurbn_dum[, ..include_urbn])
Z <- cbind(rdum_age, rgender_dum, rhh_size_dum, rjob_dum, rurbn_dum)

jid <- rep(0, nrow(Z))
for (i in 1:nrow(Z)) {
  if (sum(Z[i,]) > 0) {
    jid[i] <- last(which(Z[i,] > 0, arr.ind = TRUE))
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
  P = ncol(X),
  J = ncol(Z),
  R = max(dt_part$rep) + 1,

  # Outcome
  Y = dt_y$y,

  # Covariates
  X = X,

  # Indexes
  aid = aid,
  jid = jid,
  rid = rid,

  # Repeat effect prior
  hatGamma = config$model$hatGamma,
  hatZeta = config$model$hatZeta,
  hatEta = config$model$hatEta,

  # HSGP
  M = config$model$M,
  C = config$model$C,
  x_hsgp = x_hsgp
)

file_name <- paste("covimod_wave_21", "increp", as.character(REPEAT), sep = "_")
file_name <- paste0(file_name, ".rds")
saveRDS(stan_data, file.path("data/silver", file_name))

cat(" DONE!\n")




