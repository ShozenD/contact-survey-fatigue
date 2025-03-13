library(data.table)
library(ggplot2)

theme_set(theme_bw())

hill <- function(r, gamma, zeta, eta) {
  -gamma * exp(zeta) * r^eta / (1 + exp(zeta) * r^eta)
}

# Plot results from the longitudinal model
{
  hatGamma <- 0.95
  hatZeta <- -1.56
  hatEta <- 0.94

  r <- seq(0, 20)
  y <- hill(r, hatGamma, hatZeta, hatEta)
  plot(r, y, type = "l")
}


# Prior simulations -------------------------------------------------------
{
  n_draws <- 5e3
  gamma <- abs(rnorm(n_draws, 0, 0.1))
  zeta <- rnorm(n_draws, -0.222, 0.1)
  eta <- abs(rnorm(n_draws, 0.94, 0.1))

  r <- seq(0, 20)
  y <- sapply(1:n_draws, function(i) hill(r, gamma[i], zeta[i], eta[i]))
  p <- c(0.025, 0.10, 0.25, 0.5, 0.75, 0.90, 0.975)
  dt_rho <- as.data.table(t(apply(y, 1, function(.x) quantile2(.x, probs = p))))
  dt_rho$r <- r

  ggplot(dt_rho, aes(r, q50)) +
    geom_line(col = 4) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3, fill = 4) +
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.3, fill = 4) +
    geom_ribbon(aes(ymin = q2.5, ymax = q97.5), alpha = 0.3, fill = 4)
}
