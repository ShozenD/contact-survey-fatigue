#' Make an index set representing the position of elements from a lower triangular matrix of non-nuisance parameters
#'
#' @param A Age group
#'
#' @return A index set (matrix)
#' @export
#'
#' @examples
#' x <- make_nn_lowertri_idxset(85)
make_nn_lowertri_idxset <- function(A){
  # Make an index set for a full square matrix
  dt_full_idxset <- expand.grid(key1 = 1:A, key2 = 1:A)

  # Make an non-nuisance index set
  dt_nn_idxset <- as.data.frame(make_nn_idxset(A))

  # Make a lower triangular index set
  dt_lowertri_idxset <- as.data.frame(make_lowertri_idxset(A))

  # Combine and sort to produce lower triangular index set for a nn index set
  colnames(dt_lowertri_idxset) <- c("key1", "key2")
  dt_nn_lowertri_idxset <- merge(dt_lowertri_idxset, cbind(dt_full_idxset, dt_nn_idxset), by = c("key1", "key2"))
  dt_nn_lowertri_idxset <- dplyr::arrange(dt_nn_lowertri_idxset, V2, V1)
  dt_nn_lowertri_idxset <- dplyr::select(dt_nn_lowertri_idxset, V1, V2)

  return(as.matrix(dt_nn_lowertri_idxset))
}
