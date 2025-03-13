# Load libraries
library(readr)
library(yaml)
library(data.table)
library(mvtnorm)
library(coda)
library(ggplot2)
library(devtools)
library(utils)
library(parallel)
library(pbapply)
load_all()

# Aesthetics
theme_set(theme_bw())

# Load the data
df <- read_rds(file.path("data", "silver", "covimod_wave_4.rds"))

# Load configurations
config <- read_yaml("config/varselect.yaml")

# Preprocess the data
mh_data <- make_mh_data_varselect(df, config, remove_first_dummy = FALSE)
y <- mh_data$y
a <- mh_data$a
X <- mh_data$X
Z <- mh_data$Z

# HSGP functions
diag_spd_matern52 <- function(alpha, rho, L, M) {
  diag(2*alpha * sqrt(4.0/3) * (sqrt(5)/rho)^2.5 * 1/((sqrt(5)/rho)^2 + ((pi/2/L) * seq.int(1, M))^2)^1.5)
}

# Basis functions
hsgp_basis <- function(x, L, M) {
  B <- matrix(NA, nrow = length(x), ncol = M)
  for (m in 1:M) B[, m] <- pi/(2*L) * (x + L)
  sin(B %*% diag(seq.int(1, M)))/sqrt(L)
}

# Log-likelihood function
lpmf <- function(data, state) {
  # Unpack data
  y <- data$y
  B <- data$B
  X <- data$X
  Z <- data$Z
  L <- data$L

  # Unpack state
  b0 <- state$b0
  sigma <- state$sigma
  lenscale <- state$lenscale
  bgp <- state$bgp
  alpha <- state$alpha
  beta <- state$beta
  gamma <- state$gamma

  f <- B %*% diag_spd_matern52(sigma, lenscale, L, ncol(B)) %*% bgp
  lambda <- exp(b0 + f + X %*% alpha + Z %*% (gamma * beta))
  return(sum(dpois(y, lambda, log = TRUE)))
}

M <- 20
C <- 1.5
mh_data$L <- max(a) * C
mh_data$B <- hsgp_basis(a, mh_data$L, M)

priors <- list(
  beta0 = function(x) dnorm(x, 0, 4, log = TRUE),
  sigma = function(x) dgamma(1/x, 5, 1, log = TRUE),
  lenscale = function(x) dgamma(1/x, 5, 1, log = TRUE),
  bgp = function(x) dmvnorm(x, rep(0, M), diag(1, M), log = TRUE),
  alpha = function(x) dmvnorm(x, rep(0, ncol(X)), diag(2, ncol(X)), log = TRUE),
  beta = function(x) dnorm(x, 0, 2, log = TRUE),
  gamma = function(x) dbinom(x, 1, 0.5, log = TRUE)
)

d.b0 <- 0.1
d.sigma <- 0.25
d.lenscale <- 0.25
d.gp <- 0.01
d.a <- 0.01
d.b <- 0.2

proposals <- list(
  beta0 = function(x) runif(1, x - d.b0, x + d.b0),
  sigma = function(x) {
    sigma <- runif(1, x - d.sigma, x + d.sigma)
    if (sigma < 0) return(-sigma)
    return(sigma)
  },
  lenscale = function(x) {
    lenscale <- runif(1, x - d.lenscale, x + d.lenscale)
    if (lenscale < 0) return(-lenscale)
    return(lenscale)
  },
  bgp = function(x) as.numeric(rmvnorm(1, x, d.gp * diag(1, M))),
  alpha = function(x) as.numeric(rmvnorm(1, x, d.a * diag(1, ncol(X)))),
  beta = function(x) runif(1, x - d.b, x + d.b)
)


run_mcmc <- function(seed = 0, S = 500) {
  set.seed(seed)

  # Set initial values
  trace <- list(
    state = list(
      gamma = rep(0, ncol(Z)),
      b0 = log(mean(y)),
      sigma = 1/rgamma(1, 5, 1),
      lenscale = 1/rgamma(1, 5, 1),
      bgp = rnorm(M, 0, 0.1),
      alpha = rnorm(ncol(X), 0, 0.1),
      beta = rnorm(ncol(Z), 0, 0.1)
    ),
    trace = list(
      BETA0 = array(NA, dim = S),
      SIGMA = array(NA, dim = S),
      LENSCALE = array(NA, dim = S),
      BGP = array(NA, dim = c(S, M)),
      ALPHA = array(NA, dim = c(S, ncol(X))),
      BETA = array(NA, dim = c(S, ncol(Z))),
      GAMMA = array(NA, dim = c(S, ncol(Z)))
    ),
    accept = list(
      b0 = 0,
      sigma = 0,
      lenscale = 0,
      bgp = 0,
      alpha = 0,
      beta = rep(0, ncol(Z))
    )
  )

  for (s in 1:S) {
    # ===== Sample gamma and beta =====
    trace <- propose_gamma(s, mh_data, trace, lpmf)

    # ===== Sample beta0 =====
    trace <- propose_beta0(s, mh_data, trace, lpmf, priors$beta0, proposals$beta0)

    # ===== Sample sigma and lenscale =====
    trace <- propose_sigma(s, mh_data, trace, lpmf, priors$sigma, proposals$sigma)
    trace <- propose_lenscale(s, mh_data, trace, lpmf, priors$lenscale, proposals$lenscale)

    # ===== Sample beta_gp =====
    trace <- propose_bgp(s, mh_data, trace, lpmf, priors$bgp, proposals$bgp)

    # ===== Sample alpha and beta =====
    trace <- propose_alpha(s, mh_data, trace, lpmf, priors$alpha, proposals$alpha)
    trace <- propose_beta(s, mh_data, trace, lpmf, priors$beta, proposals$beta)
  }

  return(trace)
}

test <- run_mcmc(1, 100)

mcmc_out <- pbapply::pblapply(1:4, function(i) run_mcmc(seed = i, 1e5), cl = 4)
saveRDS(mcmc_out, "../contact-survey-fatigue-outputs/spike_slabe_wave_21.rds")
mcmc_out <- readRDS("../contact-survey-fatigue-outputs/mcmc_out.rds")

# Concatenate Chains
draws_gamma <- rbind(mcmc_out[[1]]$trace$GAMMA[-(1:5000),],
                     mcmc_out[[2]]$trace$GAMMA[-(1:5000),],
                     mcmc_out[[3]]$trace$GAMMA[-(1:5000),],
                     mcmc_out[[4]]$trace$GAMMA[-(1:5000),])

# Compute posterior inclusion probabilities
pip <- apply(draws_gamma, 2, mean)
names(pip) <- colnames(Z)
print(pip)
pip[pip > 0.5]
