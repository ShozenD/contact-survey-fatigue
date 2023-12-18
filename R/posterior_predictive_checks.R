posterior_predictive_checks <- function(fit, stan_data, config, outdir = NA) {

  if (stringr::str_detect(config$model$name, "_longit_")) { # Longitudinal model

    dt_ppc <- posterior_predictive_checks.longit(fit, stan_data)

  } else { # Cross-sectional model

    if (stringr::str_detect(config$model$name, "^zi")) {
      dt_ppc <- posterior_predictive_checks.zi(fit, stan_data)
    } else if (stringr::str_detect(config$model$name, "[^pois|^rsb]")) {
      dt_ppc <- posterior_predictive_checks.pois(fit, stan_data)
    } else if (stringr::str_detect(config$model$name, "^logit")) {
      # TODO: Implement
      warning("This feature is yet to be implemented")
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

posterior_predictive_checks.pois <- function(fit, stan_data) {
  po_draws <- fit$draws("yhat", format = "matrix")
  po_summary <- t(apply(po_draws, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))))
  colnames(po_summary) <- c("CL", "M", "CU")
  dt_ppc <- as.data.table(po_summary)

  dt_ppc$y <- c(stan_data$y0, stan_data$y1)
  dt_ppc[, inside_CI := y >= CL & y <= CU]
  cat(" The proportion of points within the 95% posterior predictive interval is:", mean(dt_ppc$inside_CI)*100, "%\n")

  return(dt_ppc)
}

posterior_predictive_checks.zi <- function(fit, stan_data) {
  po_draws <- fit$draws("yhat", format = "matrix")
  po_summary <- t(apply(po_draws, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))))
  colnames(po_summary) <- c("CL", "M", "CU")
  dt_ppc <- as.data.table(po_summary)

  dt_ppc$y <- c(stan_data$y00, stan_data$y10, stan_data$y01, stan_data$y11)
  dt_ppc[, inside_CI := y >= CL & y <= CU]
  cat(" The proportion of points within the 95% posterior predictive interval is:", mean(dt_ppc$inside_CI)*100, "%\n")

  return(dt_ppc)
}

posterior_predictive_checks.longit <- function(fit, stan_data) {
  po_draws <- fit$draws("yhat", format = "matrix")
  po_summary <- t(apply(po_draws, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))))
  colnames(po_summary) <- c("CL", "M", "CU")
  dt_ppc <- as.data.table(po_summary)

  dt_ppc$y <- stan_data$y
  dt_ppc[, inside_CI := y >= CL & y <= CU]
  cat(" The proportion of points within the 95% posterior predictive interval is:", mean(dt_ppc$inside_CI)*100, "%\n")

  return(dt_ppc)
}



