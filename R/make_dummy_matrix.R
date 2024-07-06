#' Make dummy variable matrix
#'
#' @param data Participant data
#' @param variable Variable to make dummy variables for
#' @param include Columns to include in the output matrix
#' @param ... Additional arguments to pass to fastDummies::dummy_cols
#'
#' @return
#' @export
make_dummy_matrix <- function(data, variable, include = NULL, ...) {
  data <- data.table::setDT(data)
  data <- data[, ..variable]
  data <- fastDummies::dummy_cols(data,
                                  select_columns = variable,
                                  remove_selected_columns = TRUE,
                                  omit_colname_prefix = TRUE,
                                  ...)
  if (!is.null(include)) data <- data[, ..include]
  data <- as.matrix(data)
  data[is.na(data)] <- 0

  return(data)
}
