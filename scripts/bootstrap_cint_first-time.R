library(readr)
library(purrr)
library(data.table)
library(boot)
library(devtools)
load_all()

# Load data
covimod_data <- read_rds("./data/COVIMOD/COVIMOD_data_2022-12-29.rds")

# Unpack data
dt_part <- data.table(covimod_data$part)
dt_hh <- data.table(covimod_data$hh)
dt_nhh <- data.table(covimod_data$nhh)

# ========== Data preprecoessing ==========
# Count the number of previous participations for each participant
setkeyv(dt_part, cols = c("new_id", "wave"))
dt_part[, rep := seq_len(.N) - 1, by = .(new_id)]

# Sort dt_part by new_id
dt_part <- dt_part[order(new_id)]

# Remove participants with missing values in the gender column
dt_part <- dt_part[!is.na(gender)]

# Remove participants with missing values for age and gender
dt_part <- dt_part[!is.na(age_strata)]
dt_part <- dt_part[!is.na(gender)]
dt_part <- dt_part[age_strata != "85+"]

# Impute missing ages for children
dt_part <- fill_missing_child_ages(dt_part, seed = 123)

# Preprocess household size variable
dt_part[, hh_size := ifelse(hh_p_incl_0 >= 5, "5+", hh_p_incl_0)]
dt_part[, hh_p_incl_0 := NULL]

# ===== Prepare contact count vector Y =====
# For nhh Count the number of rows (contacts) by participant and alter_age_strata
dt_nhh_sum <- dt_nhh[, .(y_nhh = .N), by = .(new_id, wave)]

# For hh sum the number of rows (contacts) by participant and alter_age_strata
dt_hh_sum <- dt_hh[, .(y_hh = sum(hh_met_this_day)), by = .(new_id, wave)]

# In dt_part, sum the values in columns Q75_u18_work to Q75_o64_else and save it as y_grp
SDcols <- colnames(dt_part)[str_detect(colnames(dt_part), "Q")]
dt_part[, y_grp := rowSums(.SD, na.rm = TRUE), .SDcols = SDcols]
dt_grp <- dt_part[, .(new_id, wave, y_grp)]

# Merge the three data.tables
dt_y <- merge(dt_grp, dt_hh_sum, by = c("new_id", "wave"), all.x = TRUE)
dt_y <- merge(dt_y, dt_nhh_sum, by = c("new_id", "wave"), all.x = TRUE)

# Replace missing values with 0
dt_y[is.na(y_grp), y_grp := 0]
dt_y[is.na(y_hh), y_hh := 0]
dt_y[is.na(y_nhh), y_nhh := 0]

# Sum the three columns to get the total number of contacts
dt_y[, y := y_grp + y_hh + y_nhh]

# Merge the y vector with the participant data
dt_part <- merge(dt_part, dt_y, by = c("new_id", "wave"))

# Truncate extreme values
dt_part[, y := ifelse(y > 30, 30, y)]
dt_cnt <- dt_part[, .(new_id, wave, imp_age, gender, hh_size, y)]

# Load weights
weights_age_gender <- read_rds("data/population_weights/age_and_gender.rds")
weights_hh_size <- read_rds("data/population_weights/hhsize.rds")

dt_cnt <- merge(dt_cnt,
                weights_age_gender,
                by.x = c("imp_age", "gender"),
                by.y = c("age", "gender"))
dt_cnt <- merge(dt_cnt,
                weights_hh_size,
                by = "hh_size",
                suffixes = c("_pop", "_hh"))

weighted_mean <- function(data, indices) {
  # Sampled data (with replacement)
  d <- data[indices,]

  # Calculate the weighted mean
  d <- d[, .(y = sum(y), n = .N), by = .(wave, imp_age, gender, hh_size)]
  d <- merge(d, weights_age_gender, by.x = c("imp_age", "gender"), by.y = c("age", "gender"))
  d[, weight := count/sum(count), by = hh_size]
  d <- d[, .(m = sum(y/n*weight)), by = hh_size]
  d <- merge(d, weights_hh_size, by = "hh_size")

  return(sum(d$m * d$weight))
}

# Select waves with at least 300 participants
setkeyv(dt_cnt, cols = c("new_id", "wave"))
dt_cnt[, rep := seq_len(.N) - 1, by = .(new_id)]
tmp <- dt_cnt[rep == 0, .(N = .N), by = wave]
select_wave <- tmp[N >= 300, wave]

dt_cnt_subset <- dt_cnt[wave %in% select_wave & rep == 0]

# Bootstrap
set.seed(0)
dt_boot <- map_dfr(select_wave, ~{
  boot_res <- boot(data = dt_cnt_subset[wave == .x], statistic = weighted_mean, R = 1000)
  ci <- boot.ci(boot_res, type = "norm")
  data.table(wave = .x, q50 = ci$t0, q2.5 = ci$normal[,2], q97.5 = ci$normal[,3])
})

write_rds(dt_boot, "../contact-survey-fatigue-outputs/results/bootstrap_cint_first-time.rds")
