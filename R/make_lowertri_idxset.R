#' Given a square index matrix, return the index set from the lower triangular part, including the diagonal
#'
#' @param A The size of one side of a squared matrix
#'
#' @return A matrix with two columns, where each row is a pair of indices
#' @export
#'
#' @examples
#' x <- make_lowertri_idxset(A)
#' X <- scale(x)
make_lowertri_idxset <- function(A) {
  x <- matrix(1:A*A, nrow = A, ncol = A)

  # Indices of the lower triangular part, excluding the diagonal
  lower_tri_indices <- which(row(x) >= col(x), arr.ind = TRUE)

  return(lower_tri_indices)
}
