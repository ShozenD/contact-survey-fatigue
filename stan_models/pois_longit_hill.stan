functions {
  // Hill function
  vector hill(vector x, real K, real S) {
    return 1 / (1 + (x / K).^S);
  }
}

data {
  int<lower=0> N;        // number of observations
  int<lower=0> N_part;   // The number of unique participants
  int<lower=0> N_wave;   // The number of waves
  int<lower=0> N_repeat; // The number of repeats
  int<lower=0> P;        // number of predictors (dummies)
  matrix[N,P] X;         // dummy variables predictor matrix
  array[N] int part_idx; // participant index
  array[N] int w_idx;    // Wave variable
  array[N] int r_idx;              // Repeat variable (may be transformed in a non-linear fashion)
  vector[N] y_lag;       // lagged response variable (AR1)

  array[N] int y;           // response variable
}

transformed data{
  // Normalise to be between [0, 1]
  vector[N] r = to_vector(r_idx);
  r = (r - min(r)) / (max(r) - min(r));

  // Adhoc fix to prevent overflow (not very elegant...)
  vector[N] log_y_lag = log(y_lag + 1);
}

parameters {
  real alpha;             // intercept
  vector[P] beta;         // dummy coefficients
  vector[N_wave] tau;     // wave coefficients
  real<lower=0> beta_rho; // hill function coefficient
  real<lower=0, upper=1> Kappa;    // Hill function: half saturation constant
  real<lower=0> Slope;    // Hill function: slope

  // Auto-regressive parameter for participant
  real phi_loc_hyper;
  real<lower=0> phi_scale_hyper;
  vector[N_part] phi_aux;        // Auxilary parameter
}

transformed parameters {
  vector[N] rho = beta_rho*hill(r, Kappa, Slope);

  vector[N_part] phi = (phi_loc_hyper + phi_aux) * phi_scale_hyper;

  // Linear predictor
  vector[N] log_lambda; // log rate
  log_lambda = alpha + X * beta + tau[w_idx] + phi[part_idx] .* log_y_lag - rho[r_idx];
}

model {
  // hyper priors
  // target += normal_lpdf(phi_loc_hyper | 0, 10);
  // target += normal_lpdf(phi_scale_hyper | 0, 1);

  // priors
  target += normal_lpdf(alpha | 0, 10);
  target += normal_lpdf(beta | 0, 1);
  target += normal_lpdf(phi_aux | 0, 1);

  // Priors for Hill function constants
  target += normal_lpdf(beta_rho | 0, 1);
  target += beta_lpdf(Kappa | 2, 2);
  target += gamma_lpdf(Slope | 3, 1);

  // Priors for random walk wave coefficients
  target += normal_lpdf(tau[1] | 0, 1);
  target += normal_lpdf(tau[2:N_wave] | tau[1:N_wave-1], 1);

  // likelihood
  target += poisson_log_lpmf(y | log_lambda);
}

generated quantities {
  array[N] int yhat;   // posterior predictions
  array[N] real log_lik; // log likelihood

  for (n in 1:N) {
    yhat[n] = poisson_log_rng(log_lambda[n]);
    log_lik[n] = poisson_log_lpmf(y[n] | log_lambda[n]);
  }
}
