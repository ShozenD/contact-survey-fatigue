# ===== Import libraries =====
library(readr)
library(data.table)
library(tidyverse)

# ===== Import data =====
repo_path <- getwd()
covimod_data <- read_rds(file.path(repo_path, "data", "COVIMOD", "COVIMOD_data_2022-12-29.rds"))

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

dt_part <- dt_part[, .(wave, new_id, date, age, age_strata, gender, hh_p_incl_0, job, school, rep, y_grp)]

# Merge the data together
dt <- merge(dt_part, dt_hh, by = c("wave", "new_id"), all.x = TRUE)
dt <- merge(dt, dt_nhh, by = c("wave", "new_id"), all.x = TRUE)

# Fill NAs with 0
dt[is.na(y_hh), y_hh := 0]
dt[is.na(y_nhh), y_nhh := 0]

# Calculate the total number of contacts
dt[, y_tot := y_hh + y_nhh + y_grp]

# Truncate household size at 4 (more than 90% of all households)
dt[, hh_size := ifelse(hh_p_incl_0 > 4, 4, hh_p_incl_0)]

# Will write with tidyverse because from here on because its much more readable
# Prevent NAs for work status
dt <- mutate(dt,
             job = case_when(
               age_strata == "0-4" & school == "None of the above" ~ "Raised-at-home-toddler",
               age_strata == "0-4" & job == "None of the above"  ~ "Raised-at-home-toddler",
               is.na(job) & school == "Nursery or pre-school" ~ school,
               is.na(job) & school == "School" ~ "Student/Pupil",
               TRUE ~ job))

# Split data into adults and children (to prevent multicolinearity)
dt <- mutate(dt,
             edu = case_when(job == "Student/Pupil" & age_strata == "5-9" ~ "Student/Pupil_6-9",
                             job == "Nursery or pre-school" ~ "Nursery or pre-school_0-5",
                             job == "Raised-at-home-toddler" ~ "Raised-at-home-toddler_0-5",
                             age_strata %in% c("0-4", "5-9", "10-14") ~ paste(job, age_strata, sep = "_"),
                             TRUE ~ "Adult"))

dt <- dt %>%
  filter(!is.na(job), !is.na(gender)) %>% # Remove gender and job NAs
  filter(age_strata != "85+") # Remove people over 85 (small sample size)

# ===== Create design matrix =====
# Make design matrix for the dummy variables
var_names <- c("edu", "age_strata", "gender", "hh_size", "job")
dt_dummies <- fastDummies::dummy_cols(select(dt, all_of(var_names)),
                                      select_columns = var_names,
                                      remove_selected_columns = TRUE)
dt_dummies$u15 <- dt$age_strata %in% c("0-4", "5-9", "10-14")

dt_dummies <- dt_dummies %>%
  select(!c(edu_Adult, `age_strata_0-4`, `age_strata_5-9`, `age_strata_10-14`,
            `job_Raised-at-home-toddler`, `job_Nursery or pre-school`)) %>%
  mutate(across(matches("job"), function(x) ifelse(u15, 0, x))) %>%
  select(!u15)

dt <- dt %>%
  arrange(new_id, wave) %>%
  group_by(new_id) %>%
  mutate(y_tot_lag = lag(y_tot, default = 0)) # First-order AR term

# Make design matrix for non-dummies
dt_non_dummies <- select(dt, new_id, wave, rep, y_tot_lag)

new_id_unique <- unique(dt_non_dummies$new_id)
part_idx <- seq(1, length(new_id_unique))
map_id_to_idx <- as.list(part_idx)
names(map_id_to_idx) <- new_id_unique

dt_non_dummies$part_idx <- unlist(map_id_to_idx[dt_non_dummies$new_id])
dt_non_dummies <- ungroup(dt_non_dummies)

part_idx <- dt_non_dummies$part_idx

# Combine the non dummies and dummy variables into one matrix
X <- cbind(select(dt_non_dummies, wave, rep, y_tot_lag), dt_dummies)
X <- as.matrix(X)

# Outcome variable
y <- dt$y_tot

data <- list(part_idx = part_idx, y = y, X = X)
saveRDS(data, file = "data/silver/covimod-wave-3to12.rds")
