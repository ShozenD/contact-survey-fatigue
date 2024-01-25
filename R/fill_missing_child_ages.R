#' Impute Missing Child Age Data
#'
#' This function imputes missing age data for child records in a given data
#' table. The age is imputed using the age strata of the record and a uniform
#' distribution within that range.
#'
#' @param dt A data.table object with columns for new_id, wave, age, and
#' age_strata
#' @param seed An optional integer for setting the random seed used in age
#' imputation
#'
#' @return A modified version of the input data.table object with missing ages
#' imputed
#'
#' @examples
#' dt <- data.table(new_id = c(1, 2, 3, 4),
#'                               age = c(2, NA, NA, 5),
#'                               age_strata = c("0-4", "5-9", "5-9", "5-9"))
#' imputed_dt <- fill_missing_child_ages(dt)
#'
#' @export
fill_missing_child_ages <- function(dt, seed = 1527) {
  set.seed(seed)
  runif.int <- function(min, max) floor(runif(1, min = min, max = max + 0.999))

  dt <- dplyr::mutate(dt,
                      min_age = as.numeric(stringr::str_extract(age_strata, "[0-9]{1,2}")),
                      max_age = as.numeric(stringr::str_extract(age_strata, "[0-9]{1,2}$")))
  imp_age <- rep(0, nrow(dt))
  for (i in 1:nrow(dt)) {
    if (is.na(dt$age[i])) {
      imp_age[i] <- runif.int(dt$min_age[i], dt$max_age[i])
    } else {
      imp_age[i] <- dt$age[i]
    }
  }
  dt$imp_age <- imp_age

  # Remove max_age and min_age from the columns
  dt <- dplyr::select(dt, -c(min_age, max_age))

  return(as.data.table(dt))
}
