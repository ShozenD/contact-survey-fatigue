#' Preprocess the age_strata and job columns to avoid complete colinearity
#'
#' @param data Participant data
#'
#' @return A data frame or date table with the age_strata and job columns preprocessed
#' @export
preproc_age_job <- function(data, drop.na = TRUE) {
  data <- setDT(data)

  # Prevent NAs for work status
  data[, job := fcase(
    age_strata == "0-4" & (school == "None of the above" | job == "None of the above"), "Raised-at-home-toddler",
    is.na(job) & school == "Nursery or pre-school", school,
    is.na(job) & school == "School", "Student/Pupil",
    rep(TRUE, .N), job
  )]

  # Re-label work status
  data[, job := fcase(
    job == "Student/Pupil", "student",
    job == "Nursery or pre-school", "preschool",
    job == "Raised-at-home-toddler", "raised_at_home",
    job == "Employed full-time (34 hours or more)", "full_time",
    job == "Employed part-time (less than 34 hours)", "part_time",
    job == "Full-time parent, homemaker", "full_time_parent",
    job == "Self employed", "self_employed",
    job == "Unemployed and not looking for a job", "unemployed_not_looking",
    job == "Unemployed but looking for a job", "unemployed_looking",
    job == "Retired", "retired",
    job == "Long-term sick or disabled", "long_term_sick",
    default = NA_character_
  )]

  if (drop.na) {
    n1 <- nrow(data)
    data <- data[!is.na(job)]
    n2 <- nrow(data)
    warning(paste("Dropped", n1 - n2, "rows with missing job"))
  }

  data[, age_strata := fifelse(
    age_strata %in% c("0-4", "5-9", "10-14"), paste(job, age_strata, sep = "_"),
    as.character(age_strata)
  )]

  data[, age_strata := fcase(
    age_strata %in% c("preschool_0-4", "preschool_5-9"), "preschool_0-5",
    age_strata %in% c("raised_at_home_0-4", "raised_at_home_5-9"), "raised_at_home_0-5",
    age_strata == "student_5-9", "student_6-9",
    rep(TRUE, .N), age_strata,
    default = NA_character_
  )]

  data[, job := fcase(
    job %in% c("preschool", "raised_at_home"), NA_character_,
    age_strata %in% c("student_6-9", "student_10-14"), NA_character_,
    rep(TRUE, .N), job,
    default = NA_character_
  )]

  levs <- c(
    "preschool_0-5", "raised_at_home_0-5", "student_6-9", "student_10-14",
    "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-69", "70-74", "75-79", "80-84"
  )
  data[, age_strata := factor(age_strata, levels = levs)]

  return(data)
}
