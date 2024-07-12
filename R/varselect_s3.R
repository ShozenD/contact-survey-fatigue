varselect_s3 <- function(data, config, selected_vars_s1, selected_vars_s2) {
  stan_data <- make_stan_data_varselect(data, config, selected_vars_s1, selected_vars_s2)
  model <- cmdstanr::cmdstan_model(file.path(getwd(), "stan_models", paste0(config$s3$name, ".stan")),
                                   compile = TRUE)
  fit <- model$sample(stan_data,
                      seed = config$s3$seed,
                      chains = config$s3$chains,
                      parallel_chains = config$s3$parallel_chains,
                      iter_warmup = config$s3$iter_warmup,
                      iter_sampling = config$s3$iter_sampling,
                      max_treedepth = config$s3$max_treedepth,
                      adapt_delta = config$s3$adapt_delta,
                      refresh = 100)

  return(fit)
}
