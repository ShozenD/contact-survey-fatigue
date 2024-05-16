#' Make stan data for variable selection models
#'
#' @param data Preprocessed data
#' @param config Configuration list
#'
#' @return Stan data list
#' @export
make_stan_data_varselect <- function(data, config){

  y <- data$y                             # Outcome vector
  D <- make_design_matrices(data)         # Design matrices
  ridx <- which(data$repeat_status == 1)  # Repeat status
  zidx <- which(data$y == 0)              # Zero outcomes
  nzidx <- which(data$y > 0)              # Non-zero outcomes

  # Initialize stan_data
  stan_data <- list(
    N = length(y),
    Nrep = length(ridx),
    Nzero = length(zidx),
    P = ncol(D$X),
    Prep = ncol(D$Xrep),
    X = D$X,
    Xrep = D$Xrep,
    ridx = ridx,
    zidx = zidx,
    nzidx = nzidx,
    y = y
  )

  stan_data <- add_horseshoe_params(stan_data, config)

  return(stan_data)
}
