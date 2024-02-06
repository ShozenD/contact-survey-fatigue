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

  w_idx <- X[, "wave"] - 2 # Since we start from wave 3
  r_idx <- X[, "rep"] + 1 # Since we start with 0 repeats
  y_lag <- X[, "y_tot_lag"]

  r <- seq(1, max(r_idx) - 1)
  w <- seq(1, max(w_idx))

  X_dummies <- X[, -c(1, 2, 3)]

  # Create stan_data
  stan_data <- list(
    N = length(y),
    N_part = max(part_idx),
    N_wave = max(w_idx),
    N_repeat = max(r_idx),
    P = ncol(X_dummies),
    X = X_dummies,
    part_idx = part_idx,
    w_idx = w_idx,
    r_idx = r_idx,
    y_lag = y_lag,
    w = w,
    r = r,
    y = y
  )

  return(stan_data)
}





