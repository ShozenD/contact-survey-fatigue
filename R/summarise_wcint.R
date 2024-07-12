#' Summarise the weighted contact intensity of different population subgroups
#'
#' @param fit A fit object from cmdstanr
#'
#' @return A data.table with the median, 2.5th and 97.5th percentiles of the contact intensity
#' @export
summarise_wcint <- function(fit) {
  # Extract posterior draws
  draws_log_m <- fit$draws("log_m", format = "matrix")
  draws_beta <- fit$draws("beta", format = "matrix")

  # Summarise contact intensity by different subgroups
  dt_cint <- rbind(
    summarise_wcint_all(draws_log_m, draws_beta),
    summarise_wcint_toddler(draws_log_m, draws_beta),
    summarise_wcint_children(draws_log_m, draws_beta),
    summarise_wcint_fulltime(draws_log_m, draws_beta),
    summarise_wcint_self_employed(draws_log_m, draws_beta),
    summarise_wcint_unemployed(draws_log_m, draws_beta),
    summarise_wcint_fulltime_parent(draws_log_m, draws_beta),
    summarise_wcint_65p(draws_log_m, draws_beta),
    summarise_wcint_female(draws_log_m, draws_beta)
  )

  return(dt_cint)
}

summarise_wcint_all <- function(draws_log_m, draws_beta) {
  log_m <- wcint_agh(0:84, draws_log_m, draws_beta)
  dt_su <- setDT(summarise_draws(exp(log_m),
                                 default_summary_measures()[2],
                                 quantiles = ~ quantile2(., probs = c(0.025, 0.975))))
  dt_su$variable <- "all"

  return(dt_su)
}

summarise_wcint_toddler <- function(draws_log_m, draws_beta) {
  dt_su <- summarise_draws(exp(wcint_agh(0:5, draws_log_m, draws_beta)),
                           posterior::default_summary_measures()[2],
                           quantiles = ~ quantile2(., probs = c(0.025, 0.975))) |> setDT()

  dt_su$variable <- "toddlers"

  return(dt_su)
}

summarise_wcint_children <- function(draws_log_m, draws_beta) {
  dt_su <- summarise_draws(exp(wcint_agh(6:18, draws_log_m, draws_beta)),
                           posterior::default_summary_measures()[2],
                           quantiles = ~ quantile2(., probs = c(0.025, 0.975))) |> setDT()

  dt_su$variable <- "children"

  return(dt_su)
}

summarise_wcint_fulltime <- function(draws_log_m, draws_beta) {
  log_m <- wcint_agh(19:64, draws_log_m, draws_beta)
  m <- exp(log_m + draws_beta[,7])

  dt_su <- setDT(summarise_draws(m,
                                 default_summary_measures()[2],
                                 quantiles = ~ quantile2(., probs = c(0.025, 0.975))))
  dt_su$variable <- "full-time"

  return(dt_su)
}

summarise_wcint_self_employed <- function(draws_log_m, draws_beta) {
  log_m <- wcint_agh(19:64, draws_log_m, draws_beta)
  m <- exp(log_m + draws_beta[,8])

  dt_su <- setDT(summarise_draws(m,
                                 default_summary_measures()[2],
                                 quantiles = ~ quantile2(., probs = c(0.025, 0.975))))
  dt_su$variable <- "self-employed"

  return(dt_su)
}

summarise_wcint_unemployed <- function(draws_log_m, draws_beta) {
  log_m <- wcint_agh(19:64, draws_log_m, draws_beta)

  # Take the average across unemployed effects
  m <- (exp(log_m + draws_beta[,11]) + exp(log_m + draws_beta[,12])) / 2
  dt_su <- setDT(summarise_draws(m,
                                 default_summary_measures()[2],
                                 quantiles = ~ quantile2(., probs = c(0.025, 0.975))))
  dt_su$variable <- "unemployed"

  return(dt_su)
}

summarise_wcint_fulltime_parent <- function(draws_log_m, draws_beta) {
  log_m <- wcint_agh(19:64, draws_log_m, draws_beta)
  m <- exp(log_m + draws_beta[,13])

  dt_su <- setDT(summarise_draws(m,
                                 default_summary_measures()[2],
                                 quantiles = ~ quantile2(., probs = c(0.025, 0.975))))
  dt_su$variable <- "full-time parent"

  return(dt_su)
}

summarise_wcint_65p <- function(draws_log_m, draws_beta) {
  log_m <- wcint_agh(65:84, draws_log_m, draws_beta)
  dt_su <- setDT(summarise_draws(exp(log_m),
                                 default_summary_measures()[2],
                                 quantiles = ~ quantile2(., probs = c(0.025, 0.975))))
  dt_su$variable <- "65+"

  return(dt_su)
}

summarise_wcint_female <- function(draws_log_m, draws_beta) {
  # Load and process population weights
  w_ag <- setDT(read_rds("data/population_weights/age_and_gender.rds"))
  w_af <- w_ag[gender == "Female"]
  w_af <- w_af[, weight := weight / sum(weight)]

  # Separate weights by gender
  w <- w_af$weight

  # Weighted log contact intensity for females and males
  log_m <- draws_log_m
  log_m_f <- sweep(log_m, 1, draws_beta[, 1], "+")
  log_m_f <- sweep(log_m_f, 2, log(w), "+")
  m <- rowSums(exp(log_m_f))

  data.table(variable = "female",
             median = median(m),
             q2.5 = quantile2(m, probs = 0.025),
             q97.5 = quantile2(m, probs = 0.975))
}
