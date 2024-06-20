# ===== Import libraries =====
library(readr)
library(sf)
library(lubridate)
library(data.table)
library(tidyverse)
library(fastDummies)

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
dt_part[y_grp > 60, y_grp := 60] # Truncate at 60 to mitigate outliers

dt_part <- dt_part[, .(wave, new_id, date, age, age_strata, gender, hh_p_incl_0, job, school, rep, y_grp, NUTS_NAME)]

# Merge the data together
dt <- merge(dt_part, dt_hh, by = c("wave", "new_id"), all.x = TRUE)
dt <- merge(dt, dt_nhh, by = c("wave", "new_id"), all.x = TRUE)

# Join NUTS3 info
df_nuts <- nuts %>%
  as.data.table() %>%
  filter(LEVL_CODE == 3, CNTR_CODE == "DE") %>%
  select(NUTS_NAME, URBN_TYPE)
dt <- merge(dt, df_nuts, by = "NUTS_NAME", all.x = TRUE)

# Fill NAs with 0
dt[is.na(y_hh), y_hh := 0]
dt[is.na(y_nhh), y_nhh := 0]

# Calculate the total number of contacts
dt[, y_tot := y_hh + y_nhh + y_grp]

# Truncate household size at 4 (more than 90% of all households)
dt[, hh_size := ifelse(hh_p_incl_0 > 5, 5, hh_p_incl_0)]

dt <- dt %>%
  filter(!is.na(age_strata), !is.na(gender)) %>% # Remove gender and job NAs
  filter(age_strata != "85+") # Remove people over 85 (small sample size)

# ===== Create design matrix =====
# Prevent NAs for work status
dt <- dt %>%
  mutate(
    job = case_when(
      age_strata == "0-4" & school == "None of the above" ~ "Raised-at-home-toddler",
      age_strata == "0-4" & job == "None of the above"  ~ "Raised-at-home-toddler",
      is.na(job) & school == "Nursery or pre-school" ~ school,
      is.na(job) & school == "School" ~ "Student/Pupil",
      TRUE ~ job)
  ) %>%
  drop_na(job) # Only drops 14 participants

## Prepare participant characteristics X
### Occupations
dt <- preproc_age_job(dt)
jobs_of_interest <- c("Full-time parent, homemaker",
                      "Long-term sick or disabled",
                      "Unemployed and not looking for a job",
                      "Unemployed but looking for a job",
                      "Retired",
                      "Self employed",
                      "Student/Pupil",
                      "Employed full-time (34 hours or more)")
dt[, job_2 := ifelse(job %in% jobs_of_interest, as.character(job), "Other")]
dt_dummy_job <- fastDummies::dummy_cols(dt[, .(new_id, job_2)],
                                        select_columns = "job_2",
                                        remove_selected_columns = TRUE,
                                        omit_colname_prefix = TRUE)
dt_dummy_job[, Other := NULL]  # Remove the Other column
dt_dummy_job[, new_id := NULL] # Remove ID column

## Prepare age related dummies
dt[, age_strata_2 := ifelse(age_strata != "85+", as.character(age_strata), "Other")]
dt_dummy_age <- fastDummies::dummy_cols(dt[, .(new_id, age_strata_2)],
                                        select_columns = "age_strata_2",
                                        remove_selected_columns = TRUE,
                                        remove_most_frequent_dummy = TRUE,
                                        omit_colname_prefix = TRUE)
dt_dummy_age[, new_id := NULL] # Remove ID column

# Household size
dt_dummy_hh <- fastDummies::dummy_cols(dt[, .(hh_size)],
                                       select_columns = "hh_size",
                                       remove_selected_columns = TRUE,
                                       remove_most_frequent_dummy = TRUE,
                                       omit_colname_prefix = TRUE)

# Gender
dt_dummy_gender <- fastDummies::dummy_cols(dt[, .(gender)],
                                           select_columns = "gender",
                                           remove_most_frequent_dummy = TRUE,
                                           omit_colname_prefix = TRUE)
dt_dummy_gender[, gender := NULL]

# Day of week
dt[, dow := lubridate::wday(date, label = TRUE)]
dt[, dow := ifelse(dow %in% c("Sat", "Sun"), "Weekend", "Weekday")]
dt_dummy_dow <- fastDummies::dummy_cols(dt[, .(dow)],
                                        select_columns = "dow",
                                        remove_selected_columns = TRUE,
                                        omit_colname_prefix = TRUE)

# Urban type
dt  <- dt[, urbn_type := case_when(URBN_TYPE == "1" ~ "Urban",
                                   URBN_TYPE == "2" ~ "Intermediate",
                                   URBN_TYPE == "3" ~ "Rural")]
dt_dummy_urban <- fastDummies::dummy_cols(dt[, .(urbn_type)],
                                          select_columns = "urbn_type",
                                          remove_selected_columns = TRUE,
                                          omit_colname_prefix = TRUE)
dt_dummy_urban[, Rural := NULL]  # Remove the Rural column

length(unique(dt$new_id))
nrow(dt)

# Combine the dummies
dt_dummy <- cbind(dt_dummy_age, dt_dummy_gender, dt_dummy_hh, dt_dummy_job, dt_dummy_dow, dt_dummy_urban)

# Make design matrix for non-dummies
dt_non_dummies <- select(dt, new_id, wave, rep)
new_id_unique <- unique(dt_non_dummies$new_id)
part_idx <- seq(1, length(new_id_unique))
map_id_to_idx <- as.list(part_idx)
names(map_id_to_idx) <- new_id_unique

dt_non_dummies$part_idx <- unlist(map_id_to_idx[dt_non_dummies$new_id])
dt_non_dummies <- ungroup(dt_non_dummies)

part_idx <- dt_non_dummies$part_idx

# Combine the non dummies and dummy variables into one matrix
X <- cbind(select(dt_non_dummies, wave, rep), dt_dummy)
X <- as.matrix(X)

# Outcome variable
y <- dt$y_tot

data <- list(part_idx = part_idx, y = y, X = X)
saveRDS(data, file = "data/silver/covimod_wave_3_12.rds")
```
