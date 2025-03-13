#' @title Make Stan data object for second stage of the variable selection
#'
#' @param data data.table
#' @param selected_vars character vector of variables selected in the first stage
#' @param fit cmdstanr fit object
#'
#' @return Stan data object (list)
#' @export
make_stan_data_varselect_s2 <- function(data, fit_s1, summary_s1) {
  ridx <- which(data$repeat_status == 1)
  data <- data[ridx, ]
  y <- data$y

  U_age <- make_design_matrix(data, "age_strata")
  U_hh <- make_design_matrix(data, "hh_size")
  U_gender <- make_design_matrix(data, "gender")
  U <- cbind(U_age, U_hh, U_gender)

  selected <- summary_s1[selected == TRUE]
  selected_job <- unique(selected[grep("job_*", varname), varname])
  selected_symp <- unique(selected[grep("symp_*", varname), varname])
  selected_dow <- unique(selected[grep("dow_*", varname), varname])
  selected_urbn <- unique(selected[grep("urbn_type_*", varname), varname])

  V_job <- make_design_matrix(data, "job")
  V_symp <- make_design_matrix(data, "symp_none")
  V_dow <- make_design_matrix(data, "dow")
  V_urbn <- make_design_matrix(data, "urbn_type")

  varnames_job <- colnames(V_job)
  varnames_symp <- colnames(V_symp)
  varnames_dow <- colnames(V_dow)
  varnames_urbn <- colnames(V_urbn)

  V_job <- V_job[, ..selected_job]
  V_symp <- V_symp[, ..selected_symp]
  V_dow <- V_dow[, ..selected_dow]
  V_urbn <- V_urbn[, ..selected_urbn]
  V <- cbind(V_job, V_symp, V_dow, V_urbn)

  W_age <- U_age
  W_hh <- U_hh
  W_gender <- U_gender
  W_job <- make_design_matrix(data, "job")
  W_urbn <- make_design_matrix(data, "urbn_type")
  W <- cbind(W_age, W_hh, W_gender, W_job, W_urbn)

  # Posterior medians from the first stage
  vars <- fit_s1$metadata()$stan_variables
  beta0 <- fit_s1$summary("beta0", median)$median
  alpha <- fit_s1$summary(vars[grep("alpha_*", vars)], median)$median
  beta_job <- fit_s1$summary(vars[grep("beta_job", vars)], median)$median[which(varnames_job %in% selected_job)]
  beta_symp <- fit_s1$summary(vars[grep("beta_symp", vars)], median)$median[which(varnames_symp %in% selected_symp)]
  beta_dow <- fit_s1$summary(vars[grep("beta_dow", vars)], median)$median[which(varnames_dow %in% selected_dow)]
  beta_urbn <- fit_s1$summary(vars[grep("beta_urbn", vars)], median)$median[which(varnames_urbn %in% selected_urbn)]
  beta <- c(beta_job, beta_symp, beta_dow, beta_urbn)

  list(
    N = length(y),
    y = y,
    Pu = ncol(U),
    U = U,
    Pv = ncol(V),
    V = V,
    Pw = ncol(W),
    W = W,

    beta0 = beta0,
    alpha = alpha,
    beta = beta
  )
}
