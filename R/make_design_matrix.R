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
make_design_matrix <- function(data, var_name, remove_first_dummy = FALSE){
  X <- fastDummies::dummy_cols(select(data, var_name),
                               select_columns = var_name,
                               ignore_na = TRUE,
                               remove_first_dummy = remove_first_dummy,
                               remove_selected_columns = TRUE)
  X[is.na(X)] <- 0

  return(X)
}
