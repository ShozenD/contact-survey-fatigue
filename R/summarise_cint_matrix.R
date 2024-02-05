#' Summarise the posterior distribution of the contact intensity matrix
#'
#' @param cint_matrix A data.table containing the posterior draws of the contact intensity matrix
#' @param probs A vector of quantiles to compute
#' @param labels A vector of labels for the quantiles
#'
#' @return A data.table containing the posterior quantiles of the contact intensity matrix
#' @import data.table
#' @export
summarise_cint_matrix <- function(cint_matrix,
                                  probs = c(0.5, 0.025, 0.25, 0.75, 0.975),
                                  labels = c('M','CL', 'Q25', 'Q50', 'CU')) {

  pattern <- "log_m\\[([0-9]+),([0-9]+)\\]"
  cint_matrix$part_age <- as.numeric(gsub(pattern, "\\1", cint_matrix$variable)) - 1
  cint_matrix$cont_age <- as.numeric(gsub(pattern, "\\2", cint_matrix$variable)) - 1

  if (!"value" %in% colnames(cint_matrix)) {
    stop("Column 'value' not found in 'cint_matrix'")
  }

  # Perform the summarisation
  cint_matrix_sum <- cint_matrix[, list(q = quantile(value, probs = probs, na.rm = TRUE),
                                     q_label = labels),
                                 by = list(part_age, cont_age)]

  cint_matrix_sum <- data.table::dcast(cint_matrix_sum, part_age + cont_age ~ q_label, value.var = "q")

  return(cint_matrix_sum)
}
