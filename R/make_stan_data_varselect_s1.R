#' Create stan data object for the first stage of the variable selection analysis
#'
#' @param data data.table
#'
#' @return A list obejct to be passed to Stan
#' @export
make_stan_data_varselect_s1 <- function(
    data,
    u_vars = c("age_strata", "hh_size", "gender"),
    v_vars = c("job", "symp_none", "dow", "urbn_type")
) {
  ridx <- which(data$repeat_status == 0)
  y <- data$y[ridx]
  data <- data[ridx,]

  U_age <- make_design_matrix(data, "age_strata")
  U_hh <- make_design_matrix(data, "hh_size")
  U_gender <- make_design_matrix(data, "gender")

  V_job <- make_design_matrix(data, "job")
  V_symp <- make_design_matrix(data, "symp_none")
  V_dow <- make_design_matrix(data, "dow")
  V_urbn <- make_design_matrix(data, "urbn_type")

  list(
    N = length(y),
    y = y,

    P_age = ncol(U_age),
    P_hh = ncol(U_hh),
    P_gender = ncol(U_gender),

    U_age = U_age,
    U_hh = U_hh,
    U_gender = U_gender,

    P_job = ncol(V_job),
    P_symp = ncol(V_symp),
    P_dow = ncol(V_dow),
    P_urbn = ncol(V_urbn),

    V_job = V_job,
    V_symp = V_symp,
    V_dow = V_dow,
    V_urbn = V_urbn
  )
}




