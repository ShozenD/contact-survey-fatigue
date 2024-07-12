#' Make stan data for variable selection models
#'
#' @param data Preprocessed data
#' @param config Configuration list
#'
#' @return Stan data list
#' @export
make_mh_data_varselect <- function(data,
                                   config,
                                   s1_results = NA,
                                   s2_results = NA,
                                   remove_first_dummy = TRUE){

  if (anyNA(s1_results)) {
    mh_data <- make_mh_data_varselect.s1(data, remove_first_dummy)
  } else if (anyNA(s2_results)) {
    mh_data <- make_stan_data_varselect.s2(data, s1_results)
  } else {
    mh_data <- make_stan_data_varselect.s3(data, s1_results, s2_results)
  }

  return(mh_data)
}

make_mh_data_varselect.s1 <- function(data, remove_first_dummy = TRUE) {
  y <- data$y
  D <- make_design_matrices(data, remove_first_dummy)

  ridx <- which(data$repeat_status == 0)
  y <- y[ridx]
  a <- D$a[ridx]
  X <- D$X[ridx, ]
  Z <- D$Z[ridx, ]

  list(y = y, a = a, X = X, Z = Z)
}

make_stan_data_varselect.s2 <- function(data, s1_results) {
  # Unpack
  selected_vars <- s1_results$selected_vars
  post_best <- s1_results$post_best
  beta0 <- post_best[variable == "beta0", median]
  beta <- post_best[variable != "beta0", median]

  y <- data$y
  X1 <- make_design_matrices(data)
  X0 <- X1[, selected_vars]

  ridx <- which(data$repeat_status == 1)
  y <- y[ridx]; X0 <- X0[ridx, ]; X1 <- X1[ridx, ];
  X1 <- cbind(rep(1, nrow(X1)), X1) # Add intercept

  colnames(X1)[1] <- "Intercept"
  stan_data <- list(
    N = length(y),
    P0 = ncol(X0),
    P1 = ncol(X1),
    X0 = X0,
    X1 = X1,
    y = y,
    beta0 = beta0,
    beta = beta
  )

  return(stan_data)
}

make_stan_data_varselect.s3 <- function(data, s1_results, s2_results) {
  # Unpack
  selected_vars_s1 <- s1_results$selected_vars
  selected_vars_s2 <- s2_results$selected_vars

  y <- data$y
  X <- make_design_matrices(data)
  X0 <- X[, selected_vars_s1]

  ridx <- which(data$repeat_status == 1)
  X1 <- X[ridx,]
  X1 <- cbind(rep(1, nrow(X1)), X1) # Add intercept
  colnames(X1)[1] <- "Intercept"
  X1 <- X1[, selected_vars_s2]

  stan_data <- list(
    N = length(y),
    rN = length(ridx),
    P0 = ncol(X0),
    P1 = ncol(X1),
    X0 = X0,
    X1 = X1,
    ridx = ridx,
    y = y
  )

  return(stan_data)
}
