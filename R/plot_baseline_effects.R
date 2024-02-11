#' Plot baseline effects of the variable selection model
#'
#' @param fit CmdStanR fit object
#' @param stan_data Stan data used to fit the model
#' @param config Configuration list
#' @param outdir Output directory
#'
#' @return A ggplot object
#' @import ggplot2
#' @importFrom stringr str_detect
#' @importFrom data.table as.data.table
#' @export
plot_baseline_effects <- function(fit, stan_data, config, outdir = NA) {
  ic_color_palette <- c("#00548F", "#7244E5", "#CC3DC7", "#CC3D5C", "#CC893D", "#A3CC3D", "#3DCC41", "#3DCCAD")

  # Extract posterior draws
  po_draws <- fit$draws(variables = "beta0", format = "matrix")

  # Convert to percentage change and compute quantile
  po_summary <- apply(po_draws, 2, function(x) quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))

  # Change rownames
  rownames(po_summary) <- c("CL", "Q25", "M", "Q75", "CU")
  df_po_summary <- data.table::as.data.table(t(po_summary))

  # Clean labels
  # If the model is poisson or rescaled beta
  if (str_detect(config$model$name, "^[pois|rsb]")) {
    cleaned_labels <- clean_labels(colnames(stan_data$X0))
  } else if (stringr::str_detect(config$model$name, "^logit")) {
    cleaned_labels <- clean_labels(colnames(stan_data$X0))
  } else {
    # Zero inflated case
    cleaned_labels <- clean_labels(colnames(stan_data$X00))
  }

  df_po_summary$variable <- cleaned_labels$variable
  df_po_summary$labels <- cleaned_labels$labels

  plt <- ggplot(df_po_summary, aes(x = labels, y = M, color = variable, shape = variable)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    geom_linerange(aes(ymin = CU, ymax = CL)) +
    geom_linerange(aes(ymin = Q75, ymax = Q25), linewidth = 1.5) +
    geom_point( fill = "white", size = 2) +
    scale_color_manual(values = ic_color_palette) +
    scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
    labs(y = expression(beta), x = "Characteristic of contacting individuals") +
    theme_bw() +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      legend.margin = margin(l = -0.2, unit = "cm"),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
    )

  ggplot2::ggsave(file.path(outdir, "baseline_effects.pdf"),
                  width = config$plot$baseline_effects$width,
                  height = config$plot$baseline_effects$height,
                  units = "cm")

  return(plt)
}
