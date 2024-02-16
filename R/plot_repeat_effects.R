#' Plot the posterior distribution of the repeat effects from the variable selection model
#'
#' @param fit A CmdStanR fit object
#' @param stan_data Stan data used to fit the model
#' @param config Configuration list
#' @param outdir Output directory
#'
#' @return A ggplot object
#' @import ggplot2
#' @importFrom posterior summarise_draws
#' @export
plot_repeat_effects <- function(fit, stan_data, config, outdir = NA) {

  if (stringr::str_detect(config$model$name, "[constrained|balanced]")) {
    plt <- plot_repeat_effects.constrained(fit, stan_data)
  } else {
    # Yet to be implemented
  }

  ggplot2::ggsave(file.path(outdir, "repeat_effects.pdf"),
                  width = config$plot$repeat_effects$width,
                  height = config$plot$repeat_effects$height,
                  units = "cm")

  return(plt)
}

plot_repeat_effects.constrained <- function(fit, stan_data) {
  ic_color_palette <- c("#00548F", "#7244E5", "#CC3DC7", "#CC3D5C", "#CC893D", "#A3CC3D", "#3DCC41", "#3DCCAD")

  po_draws <- fit$draws(variables = "beta")
  m1exp100_quantile5 <- function(x) quantile((1 - exp(-x))*100, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  df_po_summary <- summarise_draws(po_draws, ~m1exp100_quantile5(.x))

  colnames(df_po_summary) <- c("variable", "CL", "Q25", "M", "Q75", "CU")
  df_po_summary$p <- as.numeric(gsub("beta\\[([0-9]+)\\]", "\\1", df_po_summary$variable))
  df_po_summary <- filter(df_po_summary, p > stan_data$P)

  if (stringr::str_detect(config$model$name, "^[pois|rsb]")) {
    cleaned_labels <- clean_labels(colnames(stan_data$X0))
  } else if (stringr::str_detect(config$model$name, "^logit")) {
    cleaned_labels <- clean_labels(colnames(stan_data$X0))
  } else {
    # Zero inflated case
    cleaned_labels <- clean_labels(colnames(stan_data$X00))
  }

  df_po_summary$variable <- cleaned_labels$variable
  df_po_summary$labels <- cleaned_labels$labels

  plt <- ggplot(df_po_summary, aes(x = labels, y = -M, color = variable, shape = variable)) +
    geom_linerange(aes(ymin = -CU, ymax = -CL)) +
    geom_linerange(aes(ymin = -Q75, ymax = -Q25), linewidth = 1.5) +
    geom_point(fill = "white", size = 2) +
    scale_color_manual(values = ic_color_palette) +
    scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, -100, -20),
                       labels = scales::percent(seq(0, 100, 20), scale = 1),
                       limits = c(-100, 0),
                       )

  if (stringr::str_detect(config$mode$name, "^logit")) {
    plt <- plt +
      labs(y = " Percent decrease in\nodds ratio of report", x = "Characteristic of contacting individuals")
  } else {
    plt <- plt +
      labs(y = " Percent decrease in\naverage contact intensity", x = "Characteristic of contacting individuals")
  }

  plt <- plt +
    theme_bw() +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      legend.margin = margin(l = -0.2, unit = "cm"),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
    )

  return(plt)
}
