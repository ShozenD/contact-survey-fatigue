#' Summarise posterior distribution of marginal contact intensity
#'
#' @param cint_matrix A data.table containing the posterior distribution of contact intensity
#' @param use_quantile A logical indicating whether to summarise using quantiles
#' @param probs A vector of quantiles to summarise
#' @param labels A vector of labels for the quantiles
#'
#' @return A data.table containing the summary statistics posterior distribution of marginal contact intensity
#' @export
summarise_cint_marginal <- function(cint_matrix,
                                    use_quantile = TRUE,
                                    probs = c(0.5, 0.025, 0.25, 0.75, 0.975),
                                    labels = c('M','CL', 'Q25', 'Q75', 'CU')) {

  pattern <- "log_m\\[([0-9]+),([0-9]+)\\]"
  cint_matrix$part_age <- as.numeric(gsub(pattern, "\\1", cint_matrix$variable)) - 1
  cint_matrix$cont_age <- as.numeric(gsub(pattern, "\\2", cint_matrix$variable)) - 1
  cint_marginal <- cint_matrix[, .(value = sum(value)), by = .(draw, part_age)]

  if (use_quantile) {
    cint_marginal <- cint_marginal[, .(q = quantile(value, prob = probs, na.rm = TRUE), q_label = labels), by = part_age]
    cint_marginal_sum <- data.table::dcast(cint_marginal, part_age ~ q_label, value.var = "q")
  } else {
    cint_marginal_sum <- cint_marginal[, .(mean = mean(value), sd = sd(value)), by = part_age]
  }

  return(cint_marginal_sum)
}
