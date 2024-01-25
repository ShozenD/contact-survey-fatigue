#' Create a age gender grid
#'
#' This function generates a data.table with demographic groups that can be used as input
#' for a model. The data frame includes columns for age, age strata, and gender.
#'
#' @param A The maximum age in the model
#' @return A data table with demographic groups
#' @export
#'
#' @examples
#' make_grid(85)
make_age_gender_grid <- function(A) {
  age_strata <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44",
                  "45-54", "55-64", "65-69", "70-74", "75-79", "80-84")

  g <- expand.grid(age = seq(0, A - 1),
                   alter_age_strata = age_strata,
                   gender = c("Male", "Female"),
                   alter_gender = c("Male", "Female"))

  return(data.table::data.table(g))
}
