#' Makes a matrix that maps continuous age to age strata for simulated data only
#'
#' @param age_strata A vector of age strata
#' @param A The number of age groups
#' @param C The number of age strata
#'
#' @return a matrix
#' @export
make_age_strata_map_sim <- function(age_strata, A, C){

  age_strata <- sort(unique(age_strata))

  # extract minimum age for each strata
  alter_age_min <- as.numeric(str_extract(as.character(age_strata), "^[0-9]{1,2}"))

  strata_min <- unique(alter_age_min) - 5 # subtract 5 so that age starts from 0. This is a temporary fix.
  strata_min_idx <- strata_min - min(strata_min) + 1

  # create matrix to map age to age strata
  age_strata_map <- matrix(0, nrow=A, ncol=C)
  for (c in 1:C) {
    if (c == C) {
      age_strata_map[strata_min_idx[c]:A, c] <- 1
    } else {
      age_strata_map[strata_min_idx[c]:(strata_min_idx[c+1]-1), c] <- 1
    }
  }

  return(age_strata_map)
}
