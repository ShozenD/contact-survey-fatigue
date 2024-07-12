#' Update the stan data for the current step of the stepwise variable selection process
#'
#' @param i The current index of the variable to include
#' @param stage The variable selection analysis stage
#' @param stan_data The stan data list
#' @param post_init The ranked posterior summary of the initial screening model
#'
#' @return An update stan data object
#' @export
update_stan_data <- function(i, stage, stan_data, post_init) {
  if (stage == 1) {
    update_stan_data.s1(i, stan_data, post_init)
  } else if (stage == 2) {
    update_stan_data.s2(i, stan_data, post_init)
  }
}

update_stan_data.s1 <- function(i, stan_data, post_init) {
  X <- as.matrix(stan_data$X[, post_init[1:i]$vidx])

  stan_data_update <- copy(stan_data)
  stan_data_update$X <- X
  stan_data_update$P <- ncol(X)

  return(stan_data_update)
}

update_stan_data.s2 <- function(i, stan_data, post_init) {
  X1 <- as.matrix(stan_data$X1[, post_init[1:i]$vidx])

  stan_data_update <- copy(stan_data)
  stan_data_update$X1 <- X1
  stan_data_update$P1 <- ncol(X1)

  return(stan_data_update)
}
