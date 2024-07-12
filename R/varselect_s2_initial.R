#' Run initial screening for stage one of the variable selection analysis
#'
#' @param data A data table containing the preprocessed contact data
#' @param config A list containing the configurations for the variable selection analysis
#' @param s1_results A list containing the results from stage one of the variable selection analysis
#'
#' @return A data table of the posterior summary of the regression coefficients sorted by their effect size
#' @export
varselect_s2_initial <- function(data, config, s1_results) {
  stan_data <- make_stan_data_varselect(data, config, s1_results)

  config <- config$s2$init_model
  model <- cmdstanr::cmdstan_model(file.path(getwd(), "stan_models", paste0(config$name, ".stan")),
                                   compile = TRUE)
  fit <- model$sample(data = stan_data,
                      chains = config$chains,
                      parallel_chains = config$parallel_chains,
                      iter_warmup = config$iter_warmup,
                      iter_sampling = config$iter_sampling,
                      seed = config$seed,
                      show_exceptions = FALSE,
                      refresh = 500)

  post <- data.table::setDT(posterior::summarise_draws(fit$draws("gamma")))
  post[, vidx := as.numeric(stringr::str_remove_all(variable, "[gamma|\\[|\\]]"))]
  post[, varname := colnames(stan_data$X1)]
  post[, abs_eff_size := abs(median)]
  setorderv(post, cols = "abs_eff_size", order = -1)

  return(post)
}
