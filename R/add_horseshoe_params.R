add_horseshoe_params <- function(stan_data, config) {
  # Default parameters
  stan_data$hs_df <- config$model$hs_df
  stan_data$hs_df_global <- config$model$hs_df_global
  stan_data$hs_df_slab <- config$model$hs_df_slab

  # The prior guess for the number of non-zero coefficients
  stan_data$p0 <- config$model$p0

  # The scale of the slab prior (how heavy the tails are for the large coefficients)
  stan_data$hs_scale_slab <- config$model$scale_slab

  return(stan_data)
}
