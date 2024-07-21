#' Posterior Predictive Checks for Different Model Types
#'
#' This function performs posterior predictive checks on a fitted Stan model object.
#' It supports different types of models (zero-inflated, Poisson, etc.) and
#' provides a warning for unsupported model types.
#'
#' @param fit A Stan model fit object.
#' @param stan_data A list or data frame containing the data used in the
#'    model fitting process. The structure should align with what is expected
#'    by the specific model type.
#' @param type The type of model
#' @param outdir An optional directory path as a character string where the
#'    output will be saved. If `NA` (default), the function emits a warning and
#'    does not save the output.
#'
#' @return Returns a data table with the results of the posterior predictive checks.
#'    The exact structure of this table depends on the type of model used.
#'
#' @details The function checks the `model$name` field in the `config` parameter
#'    to decide which type of posterior predictive check to perform. Currently,
#'    it supports zero-inflated models (prefix "zi"), Poisson models (excluding
#'    those with prefixes "pois" and "rsb"), and has a placeholder for logistic
#'    regression models (prefix "logit"), which is not yet implemented.
#' @importFrom stringr str_detect
#' @import data.table
#' @export
posterior_predictive_checks <- function(fit, stan_data, type, outdir = NA) {

  if (stringr::str_detect(config$model$name, "_longit_")) { # Longitudinal model

    dt_ppc <- posterior_predictive_checks.longit(fit, stan_data)

  } else { # Cross-sectional model

    if (stringr::str_detect(config$model$name, "^zi")) {
      dt_ppc <- posterior_predictive_checks.zi(fit, stan_data)
    } else if (stringr::str_detect(config$model$name, "[^pois|^negb]")) {
      dt_ppc <- posterior_predictive_checks.pois(fit, stan_data)
    }
  }

  # save
  if(!is.na(outdir)){
    saveRDS(dt_ppc, file = file.path(out_dir, "ppc.rds"))
  } else {
    warning("\n outdir is not given. Results were not saved.")
  }

  return(dt_ppc)
}

posterior_predictive_checks.hurdle <- function(fit, stan_data) {
  po_draws <- fit$draws("y_rep", format = "matrix")
  po_summary <- t(apply(po_draws, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))))
  colnames(po_summary) <- c("CL", "M", "CU")
  dt_ppc <- as.data.table(po_summary)

  dt_ppc$y <- stan_data$y
  dt_ppc[, inside_CI := y >= CL & y <= CU]
  cat(" The proportion of points within the 95% posterior predictive interval is:", mean(dt_ppc$inside_CI)*100, "%\n")

  return(dt_ppc)
}

posterior_predictive_checks.zi <- function(fit, stan_data) {
  po_draws <- fit$draws("y_rep", format = "matrix")
  po_summary <- t(apply(po_draws, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))))
  colnames(po_summary) <- c("CL", "M", "CU")
  dt_ppc <- as.data.table(po_summary)

  dt_ppc$y <- stan_data$y
  dt_ppc[, inside_CI := y >= CL & y <= CU]
  cat(" The proportion of points within the 95% posterior predictive interval is:", mean(dt_ppc$inside_CI)*100, "%\n")

  return(dt_ppc)
}

posterior_predictive_checks.pois <- function(fit, stan_data) {
  po_draws <- fit$draws("y_rep", format = "matrix")
  po_summary <- t(apply(po_draws, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))))
  colnames(po_summary) <- c("CL", "M", "CU")
  dt_ppc <- as.data.table(po_summary)

  dt_ppc$y <- c(stan_data$y1, stan_data$y2)
  dt_ppc[, inside_CI := y >= CL & y <= CU]
  cat(" The proportion of points within the 95% posterior predictive interval is:", mean(dt_ppc$inside_CI)*100, "%\n")

  return(dt_ppc)
}

posterior_predictive_checks.zi <- function(fit, stan_data) {
  po_draws <- fit$draws("y_rep", format = "matrix")
  po_summary <- t(apply(po_draws, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))))
  colnames(po_summary) <- c("CL", "M", "CU")
  dt_ppc <- as.data.table(po_summary)

  dt_ppc$y <- c(stan_data$y00, stan_data$y10, stan_data$y01, stan_data$y11)
  dt_ppc[, inside_CI := y >= CL & y <= CU]
  cat(" The proportion of points within the 95% posterior predictive interval is:", mean(dt_ppc$inside_CI)*100, "%\n")

  return(dt_ppc)
}

posterior_predictive_checks.longit <- function(fit, stan_data) {
  dt_ppc <- fit$summary("y_rep", quantiles = ~ quantile2(., probs = c(0.025, 0.975)))
  dt_ppc <- as.data.table(dt_ppc)

  dt_ppc$y <- stan_data$y
  dt_ppc[, inside_CI := between(y, q2.5, q97.5)]
  cat(" The proportion of points within the 95% posterior predictive interval is:", mean(dt_ppc$inside_CI)*100, "%\n")

  return(dt_ppc)
}



