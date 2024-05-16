#' Make Design Matrix
#'
#' This function creates a design matrix from a given data frame. It processes
#' specific columns of the data frame, handling categorical variables and
#' based on a repeat status vector.
#'
#' @param df A data frame containing the variables to be processed.
#'
#' @return A design matrix with combined and processed columns.
#'
#' @examples
#' # Assuming df is a data frame with columns 'rep', 'age_strata', 'gender',
#' # 'hh_size', and 'job':
#' design_matrix <- make_design_matrix(df)
#'
#' @importFrom dplyr case_when mutate select across
#' @import fastDummies
#' @export
make_design_matrices <- function(df){
  df <- mutate(
    df,
    edu = case_when(job == "Student/Pupil" & age_strata == "5-9" ~ "Student/Pupil_6-9",
                    job == "Nursery or pre-school" ~ "Nursery or pre-school_0-5",
                    job == "Raised-at-home-toddler" ~ "Raised-at-home-toddler_0-5",
                    age_strata %in% c("0-4", "5-9", "10-14") ~ paste(job, age_strata, sep = "_"),
                    TRUE ~ "Adult")
  )

  var_names <- c("edu", "age_strata", "gender", "hh_size", "job")
  # Demographic variables
  X_demo <- fastDummies::dummy_cols(select(df, all_of(var_names)),
                                    select_columns = var_names,
                                    remove_selected_columns = TRUE)
  X_demo$u15 <- df$age_strata %in% c("0-4", "5-9", "10-14")

  X_demo <- X_demo %>%
    select(!c(edu_Adult, `age_strata_0-4`, `age_strata_5-9`, `age_strata_10-14`,
              `job_Raised-at-home-toddler`, `job_Nursery or pre-school`)) %>%
    mutate(across(matches("job"), function(x) ifelse(u15, 0, x))) %>%
    select(!u15)
  X_demo <- as.matrix(X_demo)

  # Symptoms
  X_symp <- fastDummies::dummy_columns(select(df, symp_none),
                                       select_columns = "symp_none",
                                       remove_selected_columns = TRUE)

  repeat_status <- df$repeat_status
  y <- df$y

  X <- cbind(X_demo, X_symp)        # Base variables
  Xrep <- X_demo[repeat_status == 1,] # Repeat variables

  return(list(X = X, Xrep = Xrep))
}
