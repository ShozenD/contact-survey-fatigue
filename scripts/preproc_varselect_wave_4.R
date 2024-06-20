# Libraries
library(tidyverse)
library(lubridate)
library(sf)

# Load data
repo_path <- getwd()
covimod_data <- read_rds(file.path(repo_path, "data", "COVIMOD", "COVIMOD_data_2022-12-29.rds"))
nuts <- read_sf(file.path(repo_path, "data", "NUTS_RG_20M_2021_3035.geojson"))

# Unpack data
df_part <- covimod_data$part
df_nhh <- covimod_data$nhh
df_hh <- covimod_data$hh

# ====================
# Preprocessing
# ====================
# Participants in wave 4

# Participants in wave 4 who participated before wave 4
ids_part_w4 <- df_part %>% filter(wave == 4) %>% pull(new_id) %>% unique()
ids_part_w1t4 <- df_part %>% filter(wave < 4) %>% pull(new_id) %>% unique()
ids_part_rep <- intersect(ids_part_w4, ids_part_w1t4)

df_part <- df_part %>%
  filter(wave == 4) %>%
  mutate(repeat_status = ifelse(new_id %in% ids_part_rep, 1, 0))

# Calculate number of household contacts
df_hh_cnt <- df_hh %>%
  filter(wave == 4) %>%
  group_by(new_id) %>%
  summarise(y_hh = sum(hh_met_this_day))

# Calculate the number of non-household contacts
df_nhh_cnt <- df_nhh %>%
  filter(wave == 4) %>%
  group_by(new_id) %>%
  summarise(y_nhh = n())

# Merge contact data and participant data
df_cnt <- df_part %>%
  left_join(df_hh_cnt, by = "new_id") %>%
  left_join(df_nhh_cnt, by = "new_id") %>%
  mutate(
    y_hh = ifelse(is.na(y_hh), 0, y_hh),
    y_nhh = ifelse(is.na(y_nhh), 0, y_nhh),
    y_grp = rowSums(across(Q75_u18_work:Q76_o64_else), na.rm = TRUE),
    y_grp = ifelse(y_grp > 60, 60, y_grp), # Truncate at 60
    y = y_hh + y_nhh + y_grp,
  )

# Truncate household size
df_cnt <- mutate(df_cnt, hh_size = factor(ifelse(hh_p_incl_0 <= 5, hh_p_incl_0, 5)))

# Prevent NAs for work status
df_cnt <- df_cnt %>%
  mutate(
    job = case_when(
      age_strata == "0-4" & school == "None of the above" ~ "Raised-at-home-toddler",
      age_strata == "0-4" & job == "None of the above"  ~ "Raised-at-home-toddler",
      is.na(job) & school == "Nursery or pre-school" ~ school,
      is.na(job) & school == "School" ~ "Student/Pupil",
      TRUE ~ job)
  )

# Clean job variable
df_cnt <-  mutate(
  df_cnt,
  job = case_when(
    job == "Student/Pupil" ~ "student",
    job == "Nursery or pre-school" ~ "preschool",
    job == "Raised-at-home-toddler" ~ "raised_at_home",
    job == "Employed full-time (34 hours or more)" ~ "full_time",
    job == "Employed part-time (less than 34 hours)" ~ "part_time",
    job == "Full-time parent, homemaker" ~ "full_time_parent",
    job == "Self employed" ~ "self_employed",
    job == "Unemployed and not looking for a job" ~ "unemployed_not_looking",
    job == "Unemployed but looking for a job" ~ "unemployed_looking",
    job == "Retired" ~ "retired",
    job == "Long-term sick or disabled" ~ "long_term_sick",
    TRUE ~ job
  )
)

# Create week-day week-end flag
df_cnt <- mutate(df_cnt, dow = ifelse(lubridate::wday(date, week_start = 1) > 5, "Weekend", "Weekday"))

df_cnt <- df_cnt %>%
  filter(y < 64) %>% # Remove extreme outliers
  filter(!is.na(job), !is.na(gender)) %>% # Remove gender and job NAs
  filter(age_strata != "85+") # Remove people over 85 (small sample size)

df_cnt$age_strata <- droplevels(df_cnt$age_strata)

# Join NUTS3 info
df_nuts <- nuts %>%
  as.data.frame() %>%
  filter(LEVL_CODE == 3, CNTR_CODE == "DE") %>%
  select(NUTS_NAME, URBN_TYPE)

df_cnt <- left_join(df_cnt, df_nuts, by = "NUTS_NAME")
df_cnt <- df_cnt %>% mutate(urbn_type = case_when(URBN_TYPE == 1 ~ "Urban",
                                                  URBN_TYPE == 2 ~ "Intermediate",
                                                  URBN_TYPE == 3 ~ "Rural"))

# ====================
# Save data
# ====================
saveRDS(df_cnt, file.path("data", "silver", "covimod_wave_4.rds"))

df_cnt <- mutate(
  df_cnt,
  job = case_when(
    job == "Student/Pupil" ~ "student",
    job == "Nursery or pre-school" ~ "preschool",
    job == "Raised-at-home-toddler" ~ "raised_at_home",
    job == "Employed full-time (34 hours or more)" ~ "full_time",
    job == "Employed part-time (less than 34 hours)" ~ "part_time",
    job == "Full-time parent, homemaker" ~ "full_time_parent",
    job == "Self employed" ~ "self_employed",
    job == "Unemployed and not looking for a job" ~ "unemployed_not_looking",
    job == "Unemployed but looking for a job" ~ "unemployed_looking",
    job == "Retired" ~ "retired",
    job == "long-term sick or disabled" ~ "long_term_sick",
    TRUE ~ job
  ),
  age_strata = ifelse(age_strata %in% c("0-4", "5-9", "10-14"),
                      paste(job, age_strata, sep = "_"),
                      as.character(age_strata)),
  age_strata = ifelse(age_strata %in% c("preschool_0-4", "preschool_5-9"), "preschool_0-5", age_strata),
  age_strata = ifelse(age_strata %in% c("raised_at_home_0-4", "raised_at_home_5-9"), "raised_at_home_0-5", age_strata),
  age_strata = ifelse(age_strata == "student_5-9", "student_6-9", age_strata),
  job = ifelse(job %in% c("preschool", "raised_at_home"), NA, job),
  job = ifelse(age_strata %in% c("student_6-9", "student_10-14"), NA, job)
)

df_cnt %>%
  filter(repeat_status == 1) %>%
  nrow()
