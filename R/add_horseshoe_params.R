add_horseshoe_params <- function(stan_data, stage, config) {
  if (stage == 1) {
    add_horseshoe_params.s1(stan_data, config)
  } else if (stage == 2) {
    add_horseshoe_params.s2(stan_data, config)
  }
}

add_horseshoe_params.s1 <- function(stan_data, config) {
  config <- config$s1$init_model

  # Regularised Horseshoe hyper-parameters
  stan_data$hs_df <- config$hs_df
  stan_data$hs_df_global <- config$hs_df_global
  stan_data$hs_df_slab <- config$hs_df_slab

  # The prior guess for the number of non-zero coefficients
  stan_data$p0 <- config$p0

  # The scale of the slab prior (how heavy the tails are for the large coefficients)
  stan_data$hs_scale_slab <- config$scale_slab

  return(stan_data)
}

add_horseshoe_params.s2 <- function(stan_data, config) {
  config <- config$s2$init_model

  # Regularised Horseshoe hyper-parameters
  stan_data$hs_df <- config$hs_df
  stan_data$hs_df_global <- config$hs_df_global
  stan_data$hs_df_slab <- config$hs_df_slab

  # The prior guess for the number of non-zero coefficients
  stan_data$p0 <- config$p0

  # The scale of the slab prior (how heavy the tails are for the large coefficients)
  stan_data$hs_scale_slab <- config$scale_slab

  return(stan_data)
}
