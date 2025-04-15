library(readr)
library(dplyr)

data <- read_delim("data/2000S-1002_de_flat.csv", delim = ";")

data %>%
  filter(value_unit == "Anzahl") %>%
  select(`2_variable_attribute_label`, value) %>%
  rename(
    status = `2_variable_attribute_label`,
    count = value
  ) %>%
  mutate(
    status = case_when(
      status == "Schüler/-innen u. Studierende (nicht erwerbsaktiv)" ~ "students",
      status == "Personen unterhalb des Mindestalters" ~ "under_min_age",
      status == "Erwerbstätige" ~ "employed",
      status == "Sonstige" ~ "other",
      status == "Erwerbslose" ~ "unemployed",
      status == "Empfänger/-innen von Ruhegehalt/Kapitalerträgen" ~ "retired",
      status == "Erwerbspersonen" ~ "active",
      status == "Insgesamt" ~ "total",
      status == "Nichterwerbspersonen" ~ "inactive",
      TRUE ~ NA
    )
  ) %>%
  filter(!(status %in% c("total", "inactive", "active")))
