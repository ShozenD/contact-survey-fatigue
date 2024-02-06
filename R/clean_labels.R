#' Clean the labels of the variables
#'
#' @param x A character vector of variable names
#'
#' @return A list of variables and labels
#' @importFrom stringr str_detect str_remove
#' @importFrom dplyr case_when
#' @export
clean_labels <- function(x) {
  variable <- case_when(
    stringr::str_detect(x, "^edu") ~ "Education",
    stringr::str_detect(x, "^age_strata") ~ "Age group",
    stringr::str_detect(x, "^gender") ~ "Gender",
    stringr::str_detect(x, "^hh_size") ~ "Household size",
    stringr::str_detect(x, "^job") ~ "Employment status"
  )
  variable <- factor(variable, levels = c("Age group", "Gender", "Household size", "Education", "Employment status"))

  x <- stringr::str_remove(x, "edu_")
  x <- stringr::str_remove(x, "age_strata_")
  x <- stringr::str_remove(x, "gender_")
  x <- stringr::str_remove(x, "job_")

  labels <- case_when(
    x == "Nursery or pre-school_0-5" ~ "Nursery/Pre-shool [0-5]",
    x == "Raised-at-home-toddler_0-5" ~ "Raised-at-home-toddler [0-5]",
    x == "Student/Pupil_0-5" ~ "Student [0-5]",
    x == "Student/Pupil_6-9" ~ "Student [6-9]",
    x == "Student/Pupil_10-14" ~ "Student [10-14]",
    x == "hh_size_1" ~ "1 person household",
    x == "hh_size_2" ~ "2 person household",
    x == "hh_size_3" ~ "3 person household",
    x == "hh_size_4" ~ "4 person household",
    x == "Employed full-time (34 hours or more)" ~ "Employed [full-time]",
    x == "Employed part-time (less than 34 hours)" ~ "Employed [part-time]",
    x == "Unemployed and not looking for a job" ~ "Unemployed [not seeking job]",
    x == "Unemployed but looking for a job" ~ "Unemployed [seeking job]",
    TRUE ~ x
  )
  levs <- c(labels[5:20], labels[1:4], labels[21:29])
  labels <- factor(labels, levels = levs)

  return(list(variable = variable, labels = labels))
}
