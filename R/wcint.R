#' Weighted average of contact intensity by age, gender, and household size
#'
#' @param age_set A vector of age groups
#' @param draws_log_m A draws_matrix of log contact intensity
#' @param draws_beta A draws_matrix of beta (fixed effects)
#'
#' @return
#' @export
#'
#' @examples
wcint_agh <- function(age_set, draws_log_m, draws_beta) {
  draws_log_m <- wcint_age_gender(age_set, draws_log_m, draws_beta)
  draws_log_m <- wcint_hh_size(draws_log_m, draws_beta)

  return(draws_log_m)
}

#' Weighted average of contact intensity by age and gender
#'
#' @param age_set A vector of age groups
#' @param draws_log_m A draws_matrix of log contact intensity
#' @param draws_beta A draws_matrix of beta (fixed effects)
#'
#' @return A vector of draws for the weighted contact intensity
#' @export
wcint_age_gender <- function(age_set, draws_log_m, draws_beta) {
  # Load and process population weights
  w_ag <- setDT(read_rds("data/population_weights/age_and_gender.rds"))
  w_ag <- w_ag[age %in% age_set]
  w_ag <- w_ag[, weight := count / sum(count)]

  # Separate weights by gender
  w_af <- w_ag[gender == "Female"]$weight
  w_am <- w_ag[gender == "Male"]$weight

  # Weighted log contact intensity for females and males
  log_m <- draws_log_m[, age_set + 1]
  log_m_f <- sweep(log_m, 1, draws_beta[, 1], "+")
  log_m_f <- sweep(log_m_f, 2, log(w_af), "+")
  log_m_m <- sweep(log_m, 2, log(w_am), "+")
  log_m <- log(rowSums(exp(log_m_f) + exp(log_m_m)))

  return(log_m)
}

#' Weighted average of contact intensity by household size
#'
#' @param draws_log_m a draws_matrix of log contact intensity, usually the output of wcint_age_gender.
#' @param draws_beta a draws_matrix of beta (fixed effects)
#'
#' @return A vector of draws for the weighted contact intensity
#' @export
wcint_hh_size <- function(draws_log_m, draws_beta) {
  # Load and process household size weights
  hh_weights <- setDT(read_rds("data/population_weights/hhsize.rds"))$weight

  # Calculate log_m for different household sizes
  log_m_list <- lapply(1:5, function(i) {
    if (i == 3) {
      return(draws_log_m + log(hh_weights[i]))
    } else {
      return(draws_log_m + draws_beta[, i + 1] + log(hh_weights[i]))
    }
  })

  log_m <- log(Reduce(`+`, lapply(log_m_list, exp)))

  return(log_m)
}
