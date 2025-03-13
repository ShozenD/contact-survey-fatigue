library(readr)
library(tidyselect)

# Load the data
headers <- c("year", "age", "de_Male", "de_Female", "de_total", "foreign_Male", "foreign_Female", "foreign_total")
data <- read_delim("data/de_census_population_2019.csv",
                   skip = 8,
                   n_max = 86,
                   col_names = headers,
                   delim = ";")

# Clean age column
data <- data %>%
  mutate(
    age = str_remove(age, "-J\xe4hrige"),
    age = ifelse(age == "unter 1 Jahr", "0", age),
    age = ifelse(age == "85 Jahre und mehr", "85+", age)
  )

# Subset to German nationals + foreign population
data <- data %>%
  select(age, foreign_Male, foreign_Female) %>%
  tidyr::pivot_longer(cols = starts_with("foreign"),
                      names_to = "gender",
                      values_to = "count",
                      names_pattern = "foreign_(.*)")

# Remove 85+
data <- data %>%
  filter(age != "85+") %>%
  mutate(age = as.numeric(age))

# ===== Calculate weights =====
# Weights by age and gender
weights_age_gender <- data %>%
  mutate(weight = count / sum(count))
write_rds(weights_age_gender, "data/population_weights/age_and_gender.rds")

# Weights by gender for each age
weights_gender <- data %>%
  group_by(age) %>%
  mutate(weight = count / sum(count))
write_rds(weights_gender, "data/population_weights/gender_by_age.rds")
