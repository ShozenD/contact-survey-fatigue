#' Extracts the posterior contact intensity matrix from the draws matrix
#'
#' @param draws_matrix
#'
#' @return A data.table with three columns: draws, variable, and value
#' @export
extract_cint_matrix <- function(draws_matrix) {
  alpha <- draws_matrix[, "alpha"]
  log_m <- draws_matrix[, colnames(draws_matrix) != "alpha"]

  log_cint <- sweep(log_m, 1, alpha, "+")
  cint_matrix <- exp(log_cint)
  cint_matrix <- reshape2::melt(cint_matrix)
  cint_matrix <- as.data.table(cint_matrix)

  return(cint_matrix)
}
