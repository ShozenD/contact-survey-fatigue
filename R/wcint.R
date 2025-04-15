#' Weighted average of contact intensity by age, gender, household size, urban-rural typography
#'
#' @param age_set A vector of age groups
#' @param draws_log_mu A draws_matrix of log contact intensity
#' @param draws_beta_sex A draws_matrix of beta_sex (fixed effects)
#' @param draws_beta_hhsize A draws_matrix of beta_hhsize (fixed effects)
#' @param draws_beta_urbn A draws_matrix of beta_urbn (fixed effects)
#'
#' @return A draws_array for the weighted contact intensity
#' @export
wcint_aghu <- function(age_set, draws_log_mu, draws_beta_sex, draws_beta_hhsize, draws_beta_urbn) {
  log_wcint <- wcint_age_gender(age_set, draws_log_mu, draws_beta_sex) # This function doesn't return a draws_matrix
  log_wcint <- wcint_hhsize(log_wcint, draws_beta_hhsize)
  log_wcint <- wcint_urban_rural(log_wcint, draws_beta_urbn)
  draws_log_wcint <- posterior::as_draws_matrix(matrix(log_wcint, ncol = 1))

  return(draws_log_wcint)
}

#' Weighted average of contact intensity by age and gender
#'
#' @param age_set A vector of age groups
#' @param draws_log_mu A draws_matrix of log contact intensity
#' @param draws_beta_sex A draws_matrix of beta_sex (fixed effects)
#'
#' @return A vector of draws for the weighted contact intensity
wcint_age_gender <- function(age_set, draws_log_mu, draws_beta_sex) {
  # Load and process population weights
  w_ag <- setDT(read_rds("data/population_weights/age_and_gender.rds"))[
    age %in% age_set
  ][, weight := count / sum(count)]

  # Log transform female and male weights
  w_af <- log(w_ag[gender == "Female", weight])
  w_am <- log(w_ag[gender == "Male", weight])

  # Subset draws by selected ages
  log_mu_age <- draws_log_mu[, age_set + 1]

  # Add gender effects and log weights
  log_mu_f <- sweep(log_mu_age, 1, draws_beta_sex, `+`)
  log_mu_f <- sweep(log_mu_age, 2, w_af, `+`)
  log_mu_m <- sweep(log_mu_age, 2, w_am, `+`)

  # Combine and return
  log(rowSums(exp(log_mu_f) + exp(log_mu_m)))
}

#' Weighted average of contact intensity by household size
#'
#' @param log_wcint a vector of log weighted contact intensity, usually the output of wcint_age_gender.
#' @param draws_beta a draws_matrix of beta (fixed effects)
#'
#' @return A vector of draws for the weighted contact intensity
wcint_hhsize <- function(log_wcint, draws_beta_hhsize) {
  # Load and process household size weights
  hh_weights <- setDT(readr::read_rds("data/population_weights/hhsize.rds"))$weight

  # Calculate log_mu for different household sizes
  log_wcint_list <- lapply(1:5, function(i) {
    if (i == 3) { # Reference group
      log_wcint + log(hh_weights[i])
    } else {
      if (i < 3) {
        adj <- as.numeric(draws_beta_hhsize[,i])
      } else {
        adj <- as.numeric(draws_beta_hhsize[,i - 1])
      }
      log_wcint + adj + log(hh_weights[i])
    }
  })
  log_wcint <- log(Reduce(`+`, lapply(log_wcint_list, exp)))

  return(log_wcint)
}

wcint_urban_rural <- function(log_wcint, draws_beta_urbn) {
  # Load urban_rural weights
  urbn_weights <- data.table::setDT(
    readr::read_rds("../data/population_weights/urban_rural.rds")
  )

  log_wcint_list <- lapply(1:3, function(i){
    if (i == 1) { # Reference group: urban
      log_wcint + log(urbn_weights[urbn_type == "Urban"]$weight)
    } else if (i == 2) {
      adj <- as.numeric(draws_beta_urbn)
      log_wcint + adj + log(urbn_weights[urbn_type == "Intermediate"]$weight)
    } else {
      adj <- as.numeric(draws_beta_urbn)
      log_wcint - adj + log(urbn_weights[urbn_type == "Rural"]$weight)
    }
  })

  log(Reduce(`+`, lapply(log_wcint_list, exp)))
}

wcint_job <- function(log_wcint, draws_beta_job, stan_data) {
  # Calculate weights based on sample proportions
  weights <- colMeans(stan_data$X_job)

  log_wcint_list <- lapply(1:(ncol(draws_beta_job) + 1), function(i) {
    if (i == 1) { # Reference group: unemployed
      log_wcint + log(1 - sum(weights))
    } else {
      adj <- as.numeric(draws_beta_job[, i - 1])
      log_wcint + adj + log(weights[i - 1])
    }
  })

  log(Reduce(`+`, lapply(log_wcint_list, exp)))
}
