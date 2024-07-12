#' Make Design Matrix
#'
#' This function creates a design matrix from a given data frame. It processes
#' specific columns of the data frame, handling categorical variables and
#' based on a repeat status vector.
#'
#' @param data A data frame containing the variables to be processed.
#'
#' @return A design matrix with combined and processed columns.
#'
#' @examples
#' # Assuming data is a data frame with columns 'rep', 'age_strata', 'gender',
#' # 'hh_size', and 'job':
#' design_matrix <- make_design_matrix(data)
#'
#' @importFrom dplyr case_when mutate select across
#' @import fastDummies
#' @export
make_design_matrices <- function(data, remove_first_dummy = TRUE){
  # ===== Preprocessing =====
  data <- mutate(
    data,
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

  # Impute missing age
  data <- fill_missing_child_ages(data)

  a <- scale(data$imp_age)

  var_names <- c("hh_size", "gender")
  X <- fastDummies::dummy_cols(select(data, all_of(var_names)),
                               select_columns = var_names,
                               ignore_na = TRUE,
                               remove_first_dummy = remove_first_dummy,
                               remove_selected_columns = TRUE)

  var_names <- c("job", "symp_none", "dow", "urbn_type")
  Z <- fastDummies::dummy_cols(select(data, all_of(var_names)),
                               select_columns = var_names,
                               ignore_na = TRUE,
                               remove_first_dummy = remove_first_dummy,
                               remove_selected_columns = TRUE)
  X[is.na(X)] <- 0
  Z[is.na(Z)] <- 0

  list(a = a, X = as.matrix(X), Z = as.matrix(Z))
}
