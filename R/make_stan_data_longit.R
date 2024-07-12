#' Make Stan data for longitudinal data
#'
#' @param data preprocessed data
#'
#' @return A Stan data list
#' @export
make_stan_data_longit <- function(data) {
  # Unpack data
  part_idx <- data$part_idx
  y <- data$y
  X <- data$X

  wid <- X[, "wave"] - 2 # Since we start from wave 3
  rid <- X[, "rep"] + 1 # Since we start with 0 repeats

  X_dummies <- X[, -c(1, 2, 3)]

  # Create stan_data
  stan_data <- list(
    N = length(y),
    P = ncol(X_dummies),
    X = X_dummies,
    pid = pid,
    wid = wid,
    rid = rid
  )

  return(stan_data)
}





