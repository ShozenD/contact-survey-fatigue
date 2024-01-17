#' Make convergence diagnostic statistics
#'
#' @param fit A fitted CmdStanModel
#' @param outdir Directory to save outputs
#'
#' @return A summary table of estimates and diagnostics
#' @export
make_convergence_diagnostic_stats <- function(fit, outdir = NA) {
  fit_summary <- fit$summary()

  # Effective sample size and Rhat
  ess_bulk_sum <- fit_summary$ess_bulk[!is.na(fit_summary$ess_bulk)]
  Rhat_sum <- fit_summary$rhat[!is.na(fit_summary$rhat)]

  cat("\n The minimum and maximum effective sample size are ", range(ess_bulk_sum))
  cat("\n The minimum and maximum Rhat are ", range(Rhat_sum))
  if(min(ess_bulk_sum) < 500) cat('\n Minimum effective sample size smaller than 500')

  # Diagnostics
  sampler_diagnostics <- fit$diagnostic_summary()

  # Compute WAIC and LOO
  tryCatch({
    log_lik <- fit$draws("log_lik", format = "matrix")

    n_inf_log_lik <- sum(is.infinite(log_lik))
    if(n_inf_log_lik > 0){
      .message <- paste("Detected", n_inf_log_lik, "Inf values in log_lik. Removing those iterations.")
      warning(.message)
      log_lik[is.infinite(log_lik)] <- NA
    }
    log_lik <- na.omit(log_lik)
    WAIC <- loo::waic(log_lik)
    LOO <- loo::loo(log_lik)
  }, error = function(e) e)

  # Time of execution
  time <- fit$time()

  # save
  if(!is.na(outdir)){
    saveRDS(ess_bulk_sum, file = file.path(outdir, "ess_bulk_sum.rds"))
    saveRDS(Rhat_sum, file = file.path(outdir, "Rhat_sum.rds"))
    saveRDS(WAIC, file = file.path(outdir, "WAIC.rds"))
    saveRDS(LOO, file = file.path(outdir, "LOO.rds"))
    saveRDS(sampler_diagnostics, file = file.path(outdir, "sampler_diagnostics.rds"))
    saveRDS(time, file = file.path(outdir, "time_elapsed.rds"))
  } else {
    warning("\n outdir is not given. Results were not saved.")
  }

  return(fit_summary)
}
