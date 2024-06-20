#' Preprocess the age_strata and job columns to avoid complete colinearity
#'
#' @param data Participant data
#'
#' @return A data frame or date table with the age_strata and job columns preprocessed
#' @export
preproc_age_job <- function(data, drop.na = TRUE){
  # Prevent NAs for work status
  data <- dplyr::mutate(
    data,
    job = case_when(
      age_strata == "0-4" & school == "None of the above" ~ "Raised-at-home-toddler",
      age_strata == "0-4" & job == "None of the above"  ~ "Raised-at-home-toddler",
      is.na(job) & school == "Nursery or pre-school" ~ school,
      is.na(job) & school == "School" ~ "Student/Pupil",
      TRUE ~ job)
  )

  if (drop.na) {
    n1 <- nrow(data)
    data <- dplyr::filter(data, !is.na(job))
    n2 <- nrow(data)
    warning(paste("Dropped", n1 - n2, "rows with missing job"))
  }

  data <- dplyr::mutate(
    data,
    job = dplyr::case_when(
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
}
