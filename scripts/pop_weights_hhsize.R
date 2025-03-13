library(readr)
library(tidyselect)

headers <- c("year", "1", "2", "3", "4", "5+", "total")
data <- read_delim("data/de_microcensus_household_1961_2019.csv",
                   skip = 5,
                   n_max = 58,
                   col_names = headers,
                   delim = ";")

data <- data %>%
  filter(year == "2019") %>%
  select(!total) %>%
  tidyr::pivot_longer(cols = -year, names_to = "hh_size", values_to = "count")

data <- select(data, hh_size, count)

# ===== Calculate weights =====
weights_hhsize <- data %>%
  mutate(count = as.numeric(count)) %>%
  mutate(weight = count / sum(count))

write_rds(weights_hhsize, "data/population_weights/hhsize.rds")
