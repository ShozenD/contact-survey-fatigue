library(readr)
library(readxl)
library(sf)
library(dplyr)

repo_path <- getwd()
data_pop <- read_xlsx("data/demo_r_pjangrp3$defaultview_spreadsheet.xlsx",
                  sheet = 3,
                  col_names = c("NUTS_ID", "NUTS_NAME", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
                  skip = 15)

data_geo <- read_sf(file.path(repo_path, "data", "NUTS_RG_20M_2021_3035.geojson"))

data_geo <- data_geo %>%
  filter(LEVL_CODE == 3, CNTR_CODE == "DE") %>%
  as.data.frame() %>%
  select(NUTS_ID, NUTS_NAME, URBN_TYPE)

weights <- data_geo %>%
  left_join(
    select(data_pop, "NUTS_ID", `2019`),
  by = "NUTS_ID")

weights <- weights %>%
  rename(count = `2019`) %>%
  mutate(
    count = as.numeric(count),
    urbn_type = case_when(
      URBN_TYPE == 1 ~ "Urban",
      URBN_TYPE == 2 ~ "Intermediate",
      URBN_TYPE == 3 ~ "Rural"
    )
  )

weights <- weights %>%
  group_by(urbn_type) %>%
  summarise(
    count = sum(count)
  ) %>%
  mutate(weight = count/sum(count))

write_rds(weights, "data/population_weights/urban_rural.rds")



