#' Run stepwise variable selection
#'
#' @param data A data frame with the contact data
#' @param config A list with configuration parameters
#'
#' @return A list with the posterior summary of the best fitting model and the LOO-ELPD of each model
#' @export
varselect_s2_stepwise <- function(data, config, s1_results) {
  cat( "Running initial screening model...\n")
  post_init <- varselect_s2_initial(df, config, s1_results)

  cat( "Running stepwise variable selection...\n")
  stan_data <- make_stan_data_varselect(data, config, s1_results)
  config_stepwise <- config$s2$stepwise_model
  model <- cmdstanr::cmdstan_model(file.path(getwd(), "stan_models", paste0(config_stepwise$name, ".stan")),
                                   compile = TRUE)

  loo_elpd <- rep(NA, nrow(post_init))
  fit_best <- NA
  for (i in 1:nrow(post_init)) {
    cat( " Fitting model ", i, " of ", nrow(post_init), "\n")
    stan_data_update <- update_stan_data(i, 2, stan_data, post_init)
    fit <- model$sample(stan_data_update,
                        seed = config_stepwise$seed,
                        chains = config_stepwise$chains,
                        parallel_chains = config_stepwise$parallel_chains,
                        iter_warmup = config_stepwise$iter_warmup,
                        iter_sampling = config_stepwise$iter_sampling,
                        max_treedepth = config_stepwise$max_treedepth,
                        adapt_delta = config_stepwise$adapt_delta,
                        show_exceptions = FALSE,
                        show_messages = FALSE,
                        refresh = 0)
    loo_elpd[i] <- suppressWarnings(loo::loo(fit$draws("log_lik"))$estimates[1,1]) # Extract LOO-ELPD

    if (i == 1) {
      best_elpd <- loo_elpd[i]
    } else {
      if (loo_elpd[i] > best_elpd) {
        best_elpd <- loo_elpd[i]
        fit_best <- fit
      }
    }
  }

  dt_loo_elpd <- data.table::data.table(varname = post_init$varname, loo_elpd = loo_elpd)
  selected_vars <- colnames(update_stan_data(which.max(loo_elpd), 2, stan_data, post_init)$X1)
  post_best <- data.table::setDT(posterior::summarise_draws(fit_best$draws("gamma")))
  post_best[, varname := selected_vars]

  list(
    selected_vars = selected_vars,
    post_best = post_best,
    dt_loo_elpd = dt_loo_elpd
  )
}
