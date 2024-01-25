#' Make index matrix for eigenvalues and eigenfunctions
#'
#' @param M1 The number of basis functions for the first dimension
#' @param M2 The number of basis functions for the second dimension
#'
#' @return A matrix with two columns, where each row is a pair of indices
#' @export
#'
#' @details For details, refer to section 3.2 of the paper "Practical Hilbert space approximate Bayesian Gaussian process for probabilistic programming"
#' by Riutort-Mayol et al. (2023).
make_S_matrix <- function(M1, M2){
  S <- expand.grid(seq(M1), seq(M2))
  S <- as.matrix(S)
  tmp <- S[,2]
  S[,2] <- S[,1]
  S[,1] <- tmp

  return(S)
}
