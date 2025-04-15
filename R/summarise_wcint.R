#' Summarise the weighted contact intensity of different population subgroups
#'
#' @param fit A fit object from cmdstanr
#' @param stan_data A list containing the data passed to the Stan model
#'
#' @return A data.table with the median, 2.5th and 97.5th percentiles of the contact intensity
#' @export
summarise_wcint <- function(fit, stan_data) {
  # Extract posterior draws
  draws_log_mu <- fit$draws("log_m", format = "matrix")
  draws_beta_sex <- fit$draws("beta_sex", format = "matrix")
  draws_beta_hhsize <- fit$draws("beta_hhsize", format = "matrix")
  draws_beta_job <- fit$draws("beta_job", format = "matrix")

  # Summarise contact intensity by different subgroups
  dt_cint <- rbind(
    summarise_wcint_all(
      draws_log_mu, draws_beta_sex, draws_beta_hhsize
    ),
    summarise_wcint_toddler(
      draws_log_mu, draws_beta_sex, draws_beta_hhsize
    ),
    summarise_wcint_children(
      draws_log_mu, draws_beta_sex, draws_beta_hhsize
    ),
    summarise_wcint_fulltime(
      draws_log_mu, draws_beta_sex, draws_beta_hhsize, draws_beta_job, stan_data
    ),
    summarise_wcint_self_employed(
      draws_log_mu, draws_beta_sex, draws_beta_hhsize, draws_beta_job, stan_data
    ),
    summarise_wcint_unemployed(
      draws_log_mu, draws_beta_sex, draws_beta_hhsize, draws_beta_job, stan_data
    ),
    summarise_wcint_fulltime_parent(
      draws_log_mu, draws_beta_sex, draws_beta_hhsize, draws_beta_job, stan_data
    ),
    summarise_wcint_65p(
      draws_log_mu, draws_beta_sex, draws_beta_hhsize
    ),
    summarise_wcint_female(
      draws_log_mu, draws_beta_sex
    )
  )

  return(dt_cint)
}

#' Calculate Population Average Contact Intensity
#'
#' This function calculates the population average contact intensity weighted by age, sex, and household size.
#'
#' @param draws_log_mu Numeric matrix of posterior draws of the log-transformed baseline contact intensities.
#' @param draws_beta_sex Numeric matrix of posterior draws of coefficients for the effect of sex on contact intensity.
#' @param draws_beta_hhsize Numeric matrix of posterior draws of coefficients for the effect of household size on contact intensity.
#'
#' @return A data.table containing the summarized draws of the exponential of adjusted contact intensity.
#'         The table includes summary measures and quantiles (2.5% and 97.5%) for the contact intensity,
#'         with an additional column "variable" set to "all".
#'
#' @examples
#' \dontrun{
#'   result <- summarise_wcint_all(draws_log_mu, draws_beta_sex, draws_beta_hhsize)
#'   print(result)
#' }
summarise_wcint_all <- function(
  draws_log_mu,
  draws_beta_sex,
  draws_beta_hhsize
) {
  draws_log_wcint <- wcint_agh(0:84, draws_log_mu, draws_beta_sex, draws_beta_hhsize)
  
  dt_su <- setDT(
    summarise_draws(
      exp(draws_log_wcint),
      default_summary_measures()[2],
      quantiles = ~ quantile2(., probs = c(0.025, 0.975))
    )
  )
  
  dt_su$variable <- "all"
  
  return(dt_su)
}

#' Summarise Weighted Contact Intensity for Toddlers
#'
#' This function summarizes the weighted contact intensity for toddlers by calculating
#' the exponential of the adjusted contact intensities. It uses the posterior draws of the
#' log-transformed baseline contact intensities and adjusts for sex and household size.
#'
#' @param draws_log_mu Numeric matrix of posterior draws of the log-transformed baseline contact intensities.
#' @param draws_beta_sex Numeric matrix of posterior draws of coefficients for the effect of sex on contact intensity.
#' @param draws_beta_hhsize Numeric matrix of posterior draws of coefficients for the effect of household size on contact intensity.
#'
#' @return A data.table containing summary measures of the exponentiated contact intensities,
#'         including the median, 2.5th and 97.5th percentiles, with an additional column "variable" set to "toddlers".
#'
#' @examples
#' \dontrun{
#'   result <- summarise_wcint_toddler(draws_log_mu, draws_beta_sex, draws_beta_hhsize)
#'   print(result)
#' }
summarise_wcint_toddler <- function(draws_log_mu, draws_beta_sex, draws_beta_hhsize) {
  su <- summarise_draws(
    exp(wcint_agh(0:5, draws_log_mu, draws_beta_sex, draws_beta_hhsize)),
    posterior::default_summary_measures()[2],
    quantiles = ~ quantile2(., probs = c(0.025, 0.975))
  ) |> setDT()
  
  su$variable <- "toddlers"
  
  return(su)
}

#' Summarise Weighted Contact Intensity for Children
#'
#' This function summarizes the weighted contact intensity for children by calculating
#' the exponentiated adjusted contact intensities. It uses the posterior draws of the
#' log-transformed baseline contact intensities and adjusts for sex and household size.
#'
#' @param draws_log_mu Numeric matrix of posterior draws of the log-transformed baseline contact intensities.
#' @param draws_beta_sex Numeric matrix of posterior draws of coefficients for the effect of sex on contact intensity.
#' @param draws_beta_hhsize Numeric matrix of posterior draws of coefficients for the effect of household size on contact intensity.
#'
#' @return A data.table containing summary measures of the exponentiated contact intensities,
#'         including the median, 2.5th and 97.5th percentiles, with an additional column "variable" set to "children".
#'
#' @examples
#' \dontrun{
#'   result <- summarise_wcint_children(draws_log_mu, draws_beta_sex, draws_beta_hhsize)
#'   print(result)
#' }
summarise_wcint_children <- function(draws_log_mu, draws_beta_sex, draws_beta_hhsize) {
  su <- summarise_draws(
    exp(wcint_agh(6:18, draws_log_mu, draws_beta_sex, draws_beta_hhsize)),
    posterior::default_summary_measures()[2],
    quantiles = ~ quantile2(., probs = c(0.025, 0.975))
  ) |> setDT()
  
  su$variable <- "children"
  
  return(su)
}

#' Summarise Weighted Contact Intensity for Full-Time Workers
#'
#' This function calculates the weighted contact intensity for full-time workers by computing the exponential
#' of the adjusted contact intensities. It uses the posterior draws of the log-transformed baseline contact intensities
#' and adjusts for sex, household size, and the job effect associated with full-time work.
#'
#' @param draws_log_mu Numeric matrix of posterior draws of the log-transformed baseline contact intensities.
#' @param draws_beta_sex Numeric matrix of posterior draws of coefficients for the effect of sex on contact intensity.
#' @param draws_beta_hhsize Numeric matrix of posterior draws of coefficients for the effect of household size on contact intensity.
#' @param draws_beta_job Numeric matrix of posterior draws of coefficients for the effect of job type on contact intensity.
#' @param stan_data A list containing Stan data, including "X_job", the design matrix for job effects.
#'
#' @return A data.table containing summary measures of the exponentiated contact intensities for full-time workers,
#'         including the median, 2.5th and 97.5th percentiles, with an additional column "variable" set to "full-time".
#'
#' @examples
#' \dontrun{
#'   result <- summarise_wcint_fulltime(draws_log_mu, draws_beta_sex, draws_beta_hhsize,
#'                                      draws_beta_job, stan_data)
#'   print(result)
#' }
summarise_wcint_fulltime <- function(
  draws_log_mu,
  draws_beta_sex,
  draws_beta_hhsize,
  draws_beta_job,
  stan_data
) {
  draws_log_wcint <- wcint_agh(19:64, draws_log_mu, draws_beta_sex, draws_beta_hhsize)
  
  # idx <- which(colnames(stan_data$X_job) == "full_time")
  # mu <- exp(log_mu + draws_beta_job[, idx])
  draws_wcint <- exp(draws_log_wcint)
  
  su <- setDT(
    summarise_draws(
      draws_wcint,
      default_summary_measures()[2],
      quantiles = ~ quantile2(., probs = c(0.025, 0.975))
    )
  )
  
  su$variable <- "full-time"
  return(su)
}

#' Summarise Weighted Contact Intensity for Self-Employed Individuals
#'
#' This function calculates the weighted contact intensity for self-employed individuals by adjusting
#' the baseline contact intensity for individual effects of sex and household size, and adding the job effect
#' corresponding to self-employment. The resulting contact intensity is exponentiated and summarised using
#' median and selected quantiles.
#'
#' @param draws_log_mu Numeric matrix of posterior draws of the log-transformed baseline contact intensities.
#' @param draws_beta_sex Numeric matrix of posterior draws of coefficients for the effect of sex on contact intensity.
#' @param draws_beta_hhsize Numeric matrix of posterior draws of coefficients for the effect of household size on contact intensity.
#' @param draws_beta_job Numeric matrix of posterior draws of coefficients for the effect of job type on contact intensity.
#' @param stan_data A list containing Stan data, including "X_job", the design matrix for job effects.
#'
#' @return A data.table containing summary measures of the exponentiated contact intensities for self-employed individuals,
#'         including the median, and 2.5th and 97.5th percentiles, with an additional column "variable" set to "self-employed".
#'
#' @examples
#' \dontrun{
#'   result <- summarise_wcint_self_employed(draws_log_mu, draws_beta_sex, draws_beta_hhsize,
#'                                           draws_beta_job, stan_data)
#'   print(result)
#' }
summarise_wcint_self_employed <- function(
  draws_log_mu,
  draws_beta_sex,
  draws_beta_hhsize,
  draws_beta_job,
  stan_data
) {
  # Calculate the log baseline contact intensity for ages 19 to 64 adjusted for age, sex, and household size.
  draws_log_wcint <- wcint_agh(19:64, draws_log_mu, draws_beta_sex, draws_beta_hhsize)
  
  # Identify the column in the job design matrix corresponding to self-employed individuals.
  idx <- which(colnames(stan_data$X_job) == "self_employed")
  
  # Add job effect and exponentiate to transform to contact intensity.
  draws_wcint <- exp(draws_log_wcint + draws_beta_job[, idx])
  
  # Summarise the posterior draws
  su <- setDT(
    summarise_draws(
      draws_wcint,
      default_summary_measures()[2],
      quantiles = ~ quantile2(., probs = c(0.025, 0.975))
    )
  )
  
  su$variable <- "self-employed"
  
  return(su)
}

#' Summarise Weighted Contact Intensity for Unemployed Individuals
#'
#' This function calculates the weighted contact intensity for unemployed individuals by computing the exponential
#' of the adjusted contact intensities. It uses the posterior draws of the log-transformed baseline contact intensity,
#' and adjusts for sex and household size. In addition, it incorporates the job effects for unemployed individuals,
#' averaging over those unemployed who are looking versus not looking for work.
#'
#' @param draws_log_mu Numeric matrix of posterior draws of the log-transformed baseline contact intensities.
#' @param draws_beta_sex Numeric matrix of posterior draws for the effect of sex on contact intensity.
#' @param draws_beta_hhsize Numeric matrix of posterior draws for the effect of household size on contact intensity.
#' @param draws_beta_job Numeric matrix of posterior draws for the effect of job type on contact intensity.
#' @param stan_data A list containing Stan data, including "X_job", the design matrix for job effects.
#'
#' @return A data.table containing the summary measures of the exponentiated contact intensity, including the median,
#'         2.5th and 97.5th percentiles, with an additional column "variable" set to "unemployed".
summarise_wcint_unemployed <- function(
  draws_log_mu,
  draws_beta_sex,
  draws_beta_hhsize,
  draws_beta_job,
  stan_data
) {
  draws_log_wcint <- wcint_agh(19:64, draws_log_mu, draws_beta_sex, draws_beta_hhsize)

  idx_looking <- which(colnames(stan_data$X_job) == "unemployed_looking")
  idx_not_looking <- which(colnames(stan_data$X_job) == "unemployed_not_looking")

  # Calculate the average of the unemployed effects
  mu <- (
    exp(draws_log_wcint + draws_beta_job[, idx_looking]) +
    exp(draws_log_wcint + draws_beta_job[, idx_not_looking])
  ) / 2
  
  dt_su <- setDT(
    summarise_draws(
      mu,
      default_summary_measures()[2],
      quantiles = ~ quantile2(., probs = c(0.025, 0.975))
    )
  )
  dt_su$variable <- "unemployed"
  
  return(dt_su)
}

#' Summarise Weighted Contact Intensity for Full-Time Working Parents
#'
#' This function calculates the weighted contact intensity for full-time working parents by computing the exponential
#' of the adjusted contact intensities. It uses the posterior draws of the log-transformed baseline contact intensities 
#' adjusted for age, sex, and household size, and adds the job effect corresponding to full-time working parents.
#'
#' @param draws_log_mu Numeric matrix of posterior draws of the log-transformed baseline contact intensities.
#' @param draws_beta_sex Numeric matrix of posterior draws of coefficients for the effect of sex on contact intensity.
#' @param draws_beta_hhsize Numeric matrix of posterior draws of coefficients for the effect of household size on contact intensity.
#' @param draws_beta_job Numeric matrix of posterior draws of coefficients for the effect of job type on contact intensity.
#' @param stan_data A list containing Stan data, including "X_job", the design matrix for job effects.
#'
#' @return A data.table containing summary measures (median, 2.5th, and 97.5th percentiles) of the 
#'         exponentiated weighted contact intensities for full-time working parents, with an 
#'         additional column "variable" set to "full-time parent".
#'
#' @examples
#' \dontrun{
#'   result <- summarise_wcint_fulltime_parent(draws_log_mu, draws_beta_sex, 
#'                                             draws_beta_hhsize, draws_beta_job, stan_data)
#'   print(result)
#' }
summarise_wcint_fulltime_parent <- function(
  draws_log_mu,
  draws_beta_sex,
  draws_beta_hhsize,
  draws_beta_job,
  stan_data
) {
  log_mu <- wcint_agh(19:64, draws_log_mu, draws_beta_sex, draws_beta_hhsize)
  idx <- which(colnames(stan_data$X_job) == "full_time_parent")
  mu <- exp(log_mu + draws_beta_job[, idx])
  
  su <- setDT(
    summarise_draws(
      mu,
      default_summary_measures()[2],
      quantiles = ~ quantile2(., probs = c(0.025, 0.975))
    )
  )
  
  su$variable <- "full-time parent"
  
  return(su)
}

#' Summarise Weighted Contact Intensity for 65+ Age Group
#'
#' This function calculates the weighted contact intensity for individuals aged 65 and above.
#' It uses the posterior draws of the log-transformed baseline contact intensities adjusted for 
#' age, sex, and household size. The resulting draws are exponentiated and summarised by computing 
#' the median and the 2.5th and 97.5th percentiles.
#'
#' @inheritParams summarise_wcint_all
#'
#' @return A data.table containing summary measures of the exponentiated contact intensities,
#'         including the median and the 2.5th and 97.5th percentiles, with an additional column 
#'         "variable" set to "65+".
#'
#' @examples
#' \dontrun{
#'   result <- summarise_wcint_65p(draws_log_mu, draws_beta_sex, draws_beta_hhsize)
#'   print(result)
#' }
summarise_wcint_65p <- function(
  draws_log_mu,
  draws_beta_sex,
  draws_beta_hhsize
) {
  log_mu <- wcint_agh(65:84, draws_log_mu, draws_beta_sex, draws_beta_hhsize)
  
  su <- setDT(
    summarise_draws(
      exp(log_mu),
      default_summary_measures()[2],
      quantiles = ~ quantile2(., probs = c(0.025, 0.975))
    )
  )
  
  su$variable <- "65+"
  
  return(su)
}

#' Summarise Weighted Contact Intensity for Females
#'
#' This function calculates the weighted contact intensity for females by adjusting the baseline
#' log contact intensities for the female effect and incorporating age and gender-based population weights.
#'
#' @param draws_log_mu Numeric matrix of posterior draws of the log-transformed baseline contact intensities.
#' @param draws_beta_sex Numeric matrix of posterior draws for the effect of sex on contact intensity.
#'
#' @return A data.table containing summary measures of the exponentiated contact intensities for females,
#'         including the median, 2.5th and 97.5th percentiles, with an additional column "variable" set to "female".
summarise_wcint_female <- function(draws_log_mu, draws_beta_sex) {
  # Load and process population weights
  w_ag <- setDT(read_rds("data/population_weights/age_and_gender.rds"))
  w_af <- w_ag[gender == "Female"]
  w_af[, weight := weight / sum(weight)]
  
  # Extract female-specific weights
  w <- w_af$weight
  
  # Adjust log contact intensity for the female effect and incorporate weights
  log_mu_f <- sweep(draws_log_mu, 1, draws_beta_sex, "+")
  log_mu_f <- sweep(log_mu_f, 2, log(w), "+")
  
  # Calculate total weighted contact intensity for females
  m <- rowSums(exp(log_mu_f))
  
  # Summarise the posterior draws: median, 2.5th and 97.5th percentiles
  result <- data.table(
    variable = "female",
    median = median(m),
    q2.5 = quantile2(m, probs = 0.025),
    q97.5 = quantile2(m, probs = 0.975)
  )
  
  return(result)
}
