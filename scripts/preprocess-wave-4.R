# Libraries
library(tidyverse)

# Load data
repo_path <- getwd()
covimod_data <- read_rds(file.path(repo_path, "data", "COVIMOD", "COVIMOD_data_2022-12-29.rds"))

# Unpack data
df_part <- covimod_data$part
df_nhh <- covimod_data$nhh
df_hh <- covimod_data$hh
# df_pop <- covimod_data$pop

# ====================
# Preprocessing
# ====================
# Participants in wave 4
df_part_w4 <- df_part %>% filter(wave == 4)

# Participants in wave 4 who participated before wave 4
ids_part_w4 <- df_part_w4 %>% pull(new_id) %>% unique()
ids_part_w1t4 <- df_part %>% filter(wave < 4) %>% pull(new_id) %>% unique()
ids_part_rep <- intersect(ids_part_w4, ids_part_w1t4)

df_part_w4 <- df_part_w4 %>%
  mutate(repeat_status = ifelse(new_id %in% ids_part_rep, 1, 0))

# Calculate number of household contacts
dt_hh_cnt_w4 <- df_hh %>%
  filter(wave == 4) %>%
  group_by(new_id) %>%
  summarise(y_hh = sum(hh_met_this_day))

# Calculate the number of non-household contacts
dt_nhh_cnt_w4 <- df_nhh %>%
  filter(wave == 4) %>%
  group_by(new_id) %>%
  summarise(y_nhh = n())

# Merge contact data and participant data
df_cnt_w4 <- df_part_w4 %>%
  left_join(dt_hh_cnt_w4, by = "new_id") %>%
  left_join(dt_nhh_cnt_w4, by = "new_id") %>%
  mutate(
    y_hh = ifelse(is.na(y_hh), 0, y_hh),
    y_nhh = ifelse(is.na(y_nhh), 0, y_nhh),
    y_grp = rowSums(across(Q75_u18_work:Q76_o64_else), na.rm = TRUE),
    y_grp = ifelse(y_grp > 60, 60, y_grp), # Truncate at 60
    y = y_hh + y_nhh + y_grp,
    S = y / (y + y_grp),
    S = ifelse(is.nan(S), 1, S)
  )

# Truncate group contacts and household size
df_cnt_w4 <- df_cnt_w4 %>%
  filter(S > 0, y_grp < 60) %>%
  mutate(
    hh_size = ifelse(hh_p_incl_0 <= 4, hh_p_incl_0, 4),
    hh_size = factor(hh_size)
  )

# Prevent NAs for work status
df_cnt_w4 <- df_cnt_w4 %>%
  mutate(
    job = case_when(
      age_strata == "0-4" & school == "None of the above" ~ "Raised-at-home-toddler",
      age_strata == "0-4" & job == "None of the above"  ~ "Raised-at-home-toddler",
      is.na(job) & school == "Nursery or pre-school" ~ school,
      is.na(job) & school == "School" ~ "Student/Pupil",
      TRUE ~ job)
  )

df_cnt_w4 <- df_cnt_w4 %>%
  filter(!is.na(job), !is.na(gender)) %>% # Remove gender and job NAs
  filter(age_strata != "85+") # Remove people over 85 (small sample size)

df_cnt_w4$age_strata <- droplevels(df_cnt_w4$age_strata)

# ====================
# Save data
# ====================
saveRDS(df_cnt_w4, file.path("data", "silver", "covimod-wave-4.rds"))

