#' Weighted average of contact intensity by age, gender, and household size
#'
#' @param age_set A vector of age groups
#' @param draws_log_mu A draws_matrix of log contact intensity
#' @param draws_beta_sex A draws_matrix of beta_sex (fixed effects)
#' @param draws_beta_hhsize A draws_matrix of beta_hhsize (fixed effects)
#'
#' @return A vector of draws for the weighted contact intensity
#' @export
wcint_agh <- function(age_set, draws_log_mu, draws_beta_sex, draws_beta_hhsize) {
  draws_log_mu <- wcint_age_gender(age_set, draws_log_mu, draws_beta_sex)
  draws_log_mu <- wcint_hh_size(draws_log_mu, draws_beta_hhsize)

  return(draws_log_mu)
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
  log_mu_f <- sweep(log_mu_age + draws_beta_sex[, 2], 2, w_af, `+`)
  log_mu_m <- sweep(log_mu_age + draws_beta_sex[, 1], 2, w_am, `+`)

  # Combine and return
  log(rowSums(exp(log_mu_f) + exp(log_mu_m)))
}

#' Weighted average of contact intensity by household size
#'
#' @param draws_log_m a draws_matrix of log contact intensity, usually the output of wcint_age_gender.
#' @param draws_beta a draws_matrix of beta (fixed effects)
#'
#' @return A vector of draws for the weighted contact intensity
wcint_hhsize <- function(draws_log_mu, draws_beta_hhsize) {
  # Load and process household size weights
  hh_weights <- setDT(read_rds("data/population_weights/hhsize.rds"))$weight

  # Calculate log_mu for different household sizes
  log_mu_list <- lapply(1:5, function(i){
    return(draws_log_mu + draws_beta_hhsize[,i] + log(hh_weights[i]))
  })
  log_mu <- log(Reduce(`+`, lapply(log_mu_list, exp)))

  return(log_mu)
}
