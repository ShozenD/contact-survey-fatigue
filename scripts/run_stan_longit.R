library(readr)
library(cmdstanr)
library(ggplot2)

# Load data
data <- read_rds("data/silver/covimod-wave-3to12.rds")

# Unpack data
part_idx <- data$part_idx
y <- data$y
X <- data$X

w_idx <- X[, "wave"] - 2 # Since we start from wave 3
r_idx <- X[, "rep"] + 1 # Since we start with 0 repeats
y_lag <- X[,"y_tot_lag"]

X_dummies <- X[, -c(1,2,3)]

# Create stan_data
stan_data <- list(
  N = length(y),
  N_part = max(part_idx),
  N_wave = max(w_idx),
  N_repeat = max(r_idx),
  P = ncol(X_dummies),
  X = X_dummies,
  part_idx = part_idx,
  w_idx = w_idx,
  r_idx = r_idx,
  y_lag = y_lag,
  y = y
)

stan_model <- cmdstan_model("stan_models/pois_longit_hill.stan")

fit <- stan_model$sample(data = stan_data,
                         seed = 123,
                         chains = 4,
                         parallel_chains = 4,
                         iter_warmup = 500,
                         iter_sampling = 1000,
                         max_treedepth = 11,
                         refresh = 10)

fit$save_object("../covimod-fatigue-outputs/stan_fits/pois_longit_hill.rds")

fit$summary()
fit$cmdstan_summary()

post_samples <- fit$draws(variables = c("alpha", "tau"), format = "matrix")

alpha <- post_samples[,colnames(post_samples) == "alpha"]
tau <- post_samples[,colnames(post_samples) != "alpha"]
lambda <- sweep(tau, 1, alpha, "+")

lambda <- apply(lambda, 2, function(x) quantile(exp(x), probs = c(.025, .5, .975)))
lambda <- t(lambda)

colnames(lambda) <- c("CL", "M", "CU")

df_lambda <- as.data.frame(lambda)
df_lambda$wave <- 1:nrow(df_lambda)

ggplot(df_lambda, aes(wave, M)) +
  geom_line() +
  geom_ribbon(aes(ymin = CL, ymax = CU), alpha = 0.3) +
  theme_bw()

post_samples <- fit$draws(variables = c("Kappa", "Slope"), format = "matrix")

hill_params <- apply(post_samples, 2, function(x) quantile(x, probs = c(.025, .5, .975)))
hill_params <- t(hill_params)
colnames(hill_params) <- c("CL", "M", "CU")

df_hill <- as.data.frame(hill_params)

hill <- function(x, K, S) {
  return( 1 / (1 + (K / x)^S) )
}

x <- seq(0,1,0.01)
y <- hill(x, hill_params[1,2], hill_params[2,2])
y_cl <- hill(x, hill_params[1,1], hill_params[2,1])
y_cu <- hill(x, hill_params[1,3], hill_params[2,3])

plot(x, y, type = "l")
lines(x, y_cl)
lines(x, y_cu)

ggplot(df_rho, aes(wave, M)) +
  geom_line() +
  #geom_ribbon(aes(ymin = CL, ymax = CU), alpha = 0.3) +
  theme_bw()
