#' @title Update stan_data object with selected variables
#'
#' @param stan_data the initial stan_data object (list)
#' @param stan_fit the stan_fit object
#'
#' @return A new stan_data object with the selected variables
#' @export
update_stan_data_varselect <- function(stan_data, stan_fit) {
  # Extract posterior draws for the regression coefficents
  po_draws_beta <- stan_fit$draws("beta") # Coefficients for the count model
  po_summary_beta <- as.data.table(summarise_draws(po_draws_beta, ~quantile(.x, probs = c(0.025, 0.5, 0.975))))

  po_draws_zbeta <- stan_fit$draws("zbeta") # coefficients for the zero-inflated model
  po_summary_zbeta <- as.data.table(summarise_draws(po_draws_zbeta, ~quantile(.x, probs = c(0.025, 0.5, 0.975))))

  # Add a new column to the summary tables to indicate if the 95% CI includes zero
  po_summary_beta[, zero_nin_ci := !between(0, `2.5%`, `97.5%`)]
  po_summary_zbeta[, zero_nin_ci := !between(0, `2.5%`, `97.5%`)]

  # Create a new stan_data object with the selected variables
  stan_data_updated <- copy(stan_data)
  stan_data_updated$X <- as.matrix(stan_data$X[, po_summary_beta$zero_nin_ci])
  stan_data_updated$zX <- as.matrix(stan_data$X[, po_summary_zbeta$zero_nin_ci])
  stan_data_updated$P <- ncol(stan_data_updated$X)
  stan_data_updated$zP <- ncol(stan_data_updated$zX)

  return(stan_data_updated)
}
