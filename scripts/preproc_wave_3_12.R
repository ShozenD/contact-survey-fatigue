# ===== Import libraries =====
library(readr)
library(sf)
library(lubridate)
library(data.table)
library(fastDummies)
library(devtools)
load_all()

# ===== Import data =====
repo_path <- getwd()
covimod_data <- read_rds(file.path(repo_path, "data", "COVIMOD", "COVIMOD_data_2022-12-29.rds"))
nuts <- read_sf(file.path(repo_path, "data", "NUTS_RG_20M_2021_3035.geojson"))

# Unpack data
dt_part <- setDT(covimod_data$part)
dt_nhh <- setDT(covimod_data$nhh)
dt_hh <- setDT(covimod_data$hh)

# ===== Data wrangling =====
# Compute the number of repeats for each participant
setkeyv(dt_part, cols = c("new_id", "wave"))
dt_part[, rep := seq_len(.N) - 1, by = .(new_id)]

# Filter for waves 3 to 12 (relaxed restrictions)
dt_part <- dt_part[wave >= 3 & wave <= 12]

# Count the number of household contacts by wave and participant
dt_hh <- dt_hh[, .(y_hh = sum(hh_met_this_day)), by = .(wave, new_id)]

# Count the number of non-household contacts by wave and participants
dt_nhh <- dt_nhh[, .(y_nhh = .N), by = .(wave, new_id)]

# Count the number of group contacts
SDcols_Q75 <- c("Q75_u18_work", "Q75_u18_school", "Q75_u18_else",
                "Q75_1864_work", "Q75_1864_school", "Q75_1864_else",
                "Q75_o64_work", "Q75_o64_school", "Q75_o64_else")
dt_part[, y_grp := rowSums(.SD, na.rm = T), .SDcols = SDcols_Q75]

# Merge the data together
dt <- merge(dt_part, dt_hh, by = c("wave", "new_id"), all.x = TRUE)
dt <- merge(dt, dt_nhh, by = c("wave", "new_id"), all.x = TRUE)

# Join NUTS3 info
nuts <- as.data.table(nuts)
nuts <- nuts[LEVL_CODE == 3 & CNTR_CODE == "DE", .(NUTS_NAME, URBN_TYPE)]
dt <- merge(dt, nuts, by = "NUTS_NAME", all.x = TRUE)

# Fill NAs with 0
dt[is.na(y_hh), y_hh := 0]
dt[is.na(y_nhh), y_nhh := 0]

# Urban type
dt  <- dt[, urbn_type := case_when(URBN_TYPE == "1" ~ "Urban",
                                   URBN_TYPE == "2" ~ "Intermediate",
                                   URBN_TYPE == "3" ~ "Rural")]

# Calculate the total number of contacts
dt[, y := y_hh + y_nhh + y_grp]

# Truncate household size at 4 (more than 90% of all households)
dt[, hh_size := ifelse(hh_p_incl_0 > 5, 5, hh_p_incl_0)]
dt[, hh_p_incl_0 := NULL]

dt <- dt[!is.na(age_strata) & !is.na(gender) & age_strata != "85+"] # Remove missing and 85+ age group
dt <- preproc_age_job(dt)

## Prepare participant characteristics X
# Fixed effects
X_age <- make_dummy_matrix(dt, "age_strata")
X_hh <- make_dummy_matrix(dt, "hh_size")
X_gender <- make_dummy_matrix(dt, "gender")
X_job <- make_dummy_matrix(dt, "job")
X_urbn <- make_dummy_matrix(dt, "urbn_type", include = c("Urban", "Intermediate"))

# ===== Make Stan data =====
wid <- dt$wave - min(dt$wave) + 1
rid <- dt$rep + 1

stan_data <- list(
  N = nrow(dt),

  P_age = ncol(X_age),
  P_hh = ncol(X_hh),
  P_gender = ncol(X_gender),
  P_job = ncol(X_job),
  P_urbn = ncol(X_urbn),

  X_age = X_age,
  X_hh = X_hh,
  X_gender = X_gender,
  X_job = X_job,
  X_urbn = X_urbn,

  wid = wid,
  rid = rid,
  y = dt$y
)

saveRDS(stan_data, file = "data/silver/covimod_wave_3_12.rds")
