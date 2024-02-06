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
make_design_matrices <- function(df, analysis, model_type){
  # Split data into adults and children
  df <- mutate(
    df,
    edu = case_when(job == "Student/Pupil" & age_strata == "5-9" ~ "Student/Pupil_6-9",
                    job == "Nursery or pre-school" ~ "Nursery or pre-school_0-5",
                    job == "Raised-at-home-toddler" ~ "Raised-at-home-toddler_0-5",
                    age_strata %in% c("0-4", "5-9", "10-14") ~ paste(job, age_strata, sep = "_"),
                    TRUE ~ "Adult")
  )

  var_names <- c("edu", "age_strata", "gender", "hh_size", "job")
  X <- fastDummies::dummy_cols(select(df, all_of(var_names)),
                               select_columns = var_names,
                               remove_selected_columns = TRUE)
  X$u15 <- df$age_strata %in% c("0-4", "5-9", "10-14")

  X <- X %>%
    select(!c(edu_Adult, `age_strata_0-4`, `age_strata_5-9`, `age_strata_10-14`,
              `job_Raised-at-home-toddler`, `job_Nursery or pre-school`)) %>%
    mutate(across(matches("job"), function(x) ifelse(u15, 0, x))) %>%
    select(!u15)
  X <- as.matrix(X)

  repeat_status <- df$repeat_status
  y <- df$y

  if (model_type == "zi") {
    make_design_matrices.zi(X, y, repeat_status)
  } else {
    X0 <- X[repeat_status == 0,]
    X1 <- X[repeat_status == 1,]
    return(list(X0 = X0, X1 = X1))
  }
}

# Zero-inflated model
make_design_matrices.zi <- function(X, y, repeat_status){
  X00 <- X[repeat_status == 0 & y == 0,]
  X10 <- X[repeat_status == 1 & y == 0,]
  X01 <- X[repeat_status == 0 & y > 0,]
  X11 <- X[repeat_status == 1 & y > 0,]

  Xs <- list(X00 = X00,
             X10 = X10,
             X01 = X01,
             X11 = X11)

  return(Xs)
}
