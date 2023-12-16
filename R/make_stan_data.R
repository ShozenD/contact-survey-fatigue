source("R/make_design_matrices.R")
source("R/add_horseshoe_params.R")

make_stan_data <- function(data, config){
  if (stringr::str_detect(config$model$name, "_longit_")) {
    # Data processing for longitudinal models
    stan_data <- make_stan_data.longit(data)

  } else {
    # Data processing for cross-sectional models
    if (stringr::str_detect(config$model$name, "^zi")) {
      stan_data <- make_stan_data.zi(data)

    } else if (stringr::str_detect(config$model$name, "^logit")) {
      stan_data <- make_stan_data.logit(data)

    } else if (stringr::str_detect(config$model$name, "[^pois|^rsb]")) {
      stan_data <- make_stan_data.pois(data)
    } 

    # Add horseshoe parameters
    if (stringr::str_detect(config$model$name, "horseshoe")) {
      stan_data <- add_horseshoe_params(stan_data, config)
    }
  }

  return(stan_data)
}

make_stan_data.logit <- function(data) {
  rep_stat <- data$repeat_status
  y <- as.integer(data$y_grp > 0)
  y0 <- y[rep_stat == 0]
  y1 <- y[rep_stat == 1]

  Xs <- make_design_matrices(data, "default")

  stan_data <- list(
    N0 = nrow(Xs[[1]]),
    N1 = nrow(Xs[[2]]),
    P = ncol(Xs[[1]]),
    X0 = Xs[[1]],
    X1 = Xs[[2]],
    y0 = y0,
    y1 = y1
  )

  return(stan_data)
}

make_stan_data.pois <- function(data) {
  rep_stat <- data$repeat_status
  y <- data$y
  y0 <- y[rep_stat == 0]
  y1 <- y[rep_stat == 1]

  Xs <- make_design_matrices(data, "default")

  stan_data <- list(
    N0 = nrow(Xs[[1]]),
    N1 = nrow(Xs[[2]]),
    P = ncol(Xs[[1]]),
    X0 = Xs[[1]],
    X1 = Xs[[2]],
    y0 = y0,
    y1 = y1
  )

  return(stan_data)
}

make_stan_data.zi <- function(data) {
  # Repeat status of the participant
  rep_stat <- data$repeat_status

  # Outcome vectors
  y <- data$y
  y00 <- y[rep_stat == 0 & y == 0]
  y10 <- y[rep_stat == 1 & y == 0]
  y01 <- y[rep_stat == 0 & y > 0]
  y11 <- y[rep_stat == 1 & y > 0]

  Xs <- make_design_matrices(data, model_type = "zi")

  stan_data <- list(
    N00 = length(y00),
    N10 = length(y10),
    N01 = length(y01),
    N11 = length(y11),

    y00 = y00,
    y10 = y10,
    y01 = y01,
    y11 = y11,

    X00 = Xs$X00,
    X10 = Xs$X10,
    X01 = Xs$X01,
    X11 = Xs$X11,

    P = ncol(Xs$X00)
  )

  return(stan_data)
}

make_stan_data.longit <- function(data) {
  # Unpack data
  part_idx <- data$part_idx
  y <- data$y
  X <- data$X

  w_idx <- X[, "wave"] - 2 # Since we start from wave 3
  r_idx <- X[, "rep"] + 1 # Since we start with 0 repeats
  y_lag <- X[, "y_tot_lag"]

  r <- seq(1, max(r_idx) - 1)

  X_dummies <- X[, -c(1, 2, 3)]

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
    r = r,
    y = y
  )

  return(stan_data)
}





