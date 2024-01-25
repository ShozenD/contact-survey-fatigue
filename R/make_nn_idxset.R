#' Compute the indices of the non-nuisance parameters for the difference in age parameterization
#'
#' @param A Number of age groups
#'
#' @return An index set. A matrix with two columns, where each row contains a pair of indices.
#' @export
#'
#' @examples
#' nn_indices <- make_nn_idx_set(30)
#' X <- scale(nn_indices)
make_nn_idxset <- function(A) {
  nn_indices <- matrix(NA, nrow = A*A, ncol = 2)
  n <- 1
  for (i in 1:A) {
    for (j in 1:A) {
      nn_indices[n,1] <- A - i + j
      nn_indices[n,2] <- i

      n <- n + 1
    }
  }

  return(nn_indices)
}
