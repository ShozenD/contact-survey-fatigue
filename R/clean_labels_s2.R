#' Clean labels for the second stage of the variable selection analysis
#'
#' @param su posterior summary
#' @param stan_data Stan data used to fit the model
#'
#' @return data.table
#' @export
clean_labels_s2 <- function(su, stan_data) {
  su$varname <- colnames(stan_data$W)
  su[, variable := stringr::str_split(varname, "_", simplify = TRUE)[, 1]]

  .pattern <- "(age_strata_student|age_strata|gender|hh_size|job|symp_none|dow|urbn_type)_"
  su[, category := stringr::str_remove(varname, .pattern)]
  su[, category := case_when(
    category == "preschool_0-5" ~ "Preschool (0-5)",
    category == "raised_at_home_0-5" ~ "Raised at home (0-5)",
    category == "1" ~ "1 person",
    category == "2" ~ "2 person",
    category == "3" ~ "3 person",
    category == "4" ~ "4 person",
    category == "5" ~ "5+ person",
    category == "full_time" ~ "Full-time employed",
    category == "part_time" ~ "Part-time employed",
    category == "long_term_sick" ~ "Long-term sick",
    category == "self_employed" ~ "Self-employed",
    category == "student" ~ "Student",
    category == "full_time_parent" ~ "Stay-at-home parent",
    category == "retired" ~ "Retired",
    category == "unemployed_looking" ~ "Unemployed (seeking)",
    category == "unemployed_not_looking" ~ "Unemployed (not seeking)",
    category == "No" ~ "Yes",
    category == "Yes" ~ "No",
    TRUE ~ category
  )]
  su[, variable := case_when(
    variable == "age" ~ "Age group",
    variable == "gender" ~ "Gender",
    variable == "hh" ~ "Household size",
    variable == "job" ~ "Employment",
    variable == "symp" ~ "Symptoms",
    variable == "dow" ~ "Day of week",
    variable == "urbn" ~ "Urban type"
  )]
  levs <- c("Age group", "Gender", "Household size", "Employment", "Symptoms", "Day of week", "Urban type")
  su$variable <- factor(su$variable, levels = levs)

  age.levs <- c("Preschool (0-5)", "Raised at home (0-5)",
                "6-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64",
                "65-69", "70-74", "75-79", "80-84")
  gender.levs <- c("Male", "Female")
  hh.levs <- c("1 person", "2 person", "3 person", "4 person", "5+ person")
  job.levs <- c("Full-time employed", "Part-time employed", "Self-employed", "Student", "Retired", "Long-term sick",
                "Unemployed (seeking)", "Unemployed (not seeking)", "Stay-at-home parent")
  symp.levs <- c("Yes", "No")
  dow.levs <- c("Weekday", "Weekend")
  urbn.levs <- c("Rural", "Intermediate", "Urban")
  su$category <- factor(su$category,
                        levels = c(age.levs, gender.levs, hh.levs, job.levs, symp.levs, dow.levs, urbn.levs))
  return(su)
}
