ITER_WARMUP <- 5000
ITER_MAX <- 10000 # Number of iterations
THETA <- matrix(0, nrow = ITER_MAX, ncol = ncol(X2) + ncol(X_bspline)) # Matrix to save parameter values

# Density function of the generalised double Pareto distribution
dgdP <- function(x, alpha, zeta, log = FALSE) {
  y <- 1/(2*zeta) * (1 + abs(x)/(alpha*zeta))^(-alpha + 1)
  if (!log) {
    return(y)
  } else {
    return(log(y))
  }
}

set.seed(1)
{
  acs <- 0
  beta <- rep(0, ncol(X2))
  beta_bspline <- rep(0, ncol(X_bspline))
  pmn_beta <- rep(0, ncol(X2))
  psd_beta <- rep(10, ncol(X2))
  pmn_beta_spline <- rep(0, ncol(X_bspline))
  alpha <- 1
  eta <- 1

  for (s in 1:ITER_MAX) {
    beta_new <- t( rmvnorm(1, beta, 8e-4*diag(ncol(X2)) ))
    beta_bspline_new <- t( rmvnorm(1, beta_bspline, 3e-3*diag(ncol(X_bspline))) )

    lambda_old <- exp(X2 %*% beta + X_bspline %*% beta_bspline + log(S))
    lambda_new <- exp(X2 %*% beta_new + X_bspline %*% beta_bspline_new + log(S))

    loglik_old <- sum(dpois(y, lambda_old, log = T)) +
      sum(dnorm(beta, pmn_beta, psd_beta, log = T)) +
      sum(dgdP(beta_bspline, 1, 1, log = T))
    loglik_new <- sum(dpois(y, lambda_new, log = T)) +
      sum(dnorm(beta_new, pmn_beta, psd_beta, log = T)) +
      sum(dgdP(beta_bspline_new, 1, 1, log = T))

    lhr <- loglik_new - loglik_old
    if (log(runif(1)) < lhr) {
      beta <- beta_new
      beta_bspline <- beta_bspline_new
      acs <- acs + 1
    }

    THETA[s,] <- c(beta, beta_bspline)
  }

  print(paste0("Acceptance probability: ", acs/iter_max * 100, "%"))
}