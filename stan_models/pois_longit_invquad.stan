functions {
  /* Gaussian process with squared exponential covariance function */
  // x: input vector
  // mu: auxiliary random variable vector
  // scale: scale parameter
  // lengthscale: lengthscale parameter
  // nugget: nugget parameter
  vector gp_se(array[] real x, vector mu, real scale, real lenscale, real nugget) {
    int N = num_elements(x);
    matrix[N, N] K = gp_exp_quad_cov(x, scale, lenscale);
    K = K + diag_matrix(rep_vector(square(nugget), N));
    matrix[N, N] L = cholesky_decompose(K);

    return L * mu;
  }

  /* Gaussian process with Matern 3/2 covariance functions */
  vector gp_matern32(array[] real x, vector mu, real scale, real lenscale, real nugget) {
    int N = num_elements(x);
    matrix[N, N] K = gp_matern32_cov(x, scale, lenscale);
    K = K + diag_matrix(rep_vector(square(nugget), N));
    matrix[N, N] L = cholesky_decompose(K);

    return L * mu;
  }

  /* Logistic function */
  vector logistic(vector x, real alpha, real beta, real gamma) {
    return - gamma * exp(alpha) * x.^beta ./ (1 + exp(alpha) * x.^beta);
  }

  /* Inverse quadratic function */
  vector inv_quad(vector x, real alpha, real beta, real gamma) {
    return -gamma + gamma ./ (1 + alpha*x + beta*x.^2);
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
  array[N] int r_idx;    // Repeat variable (may be transformed in a non-linear fashion)
  vector[N] y_lag;       // lagged response variable (AR1)

  array[N_wave] real w;     // A array of the number of waves
  vector[N_repeat-1] r;     // A array of the number of repeats

  array[N] int y;           // response variable
}

transformed data{
  // Adhoc fix to prevent overflow (not very elegant...)
  vector[N] log_y_lag = log(y_lag + 1);
}

parameters {
  real alpha;                // Intercept
  vector[P] beta;            // dummy coefficients

  // Time varying coefficients
  // vector[N_wave] tau;
  vector[N_wave] gp_time_mu;
  real<lower=0> gp_time_scale;
  real<lower=0> gp_time_lenscale;

  real<lower=0> alpha_repeat;    // Logistic function: intercept
  real<lower=0> beta_repeat;     // Logistic function: coefficient for the log term
  real<lower=0> gamma_repeat;    // Logistic function: scale parameter

  // Auto-regressive parameter for participant
  real phi_loc_hyper;
  real<lower=0> phi_scale_hyper;
  vector[N_part] phi_aux;        // Auxilary parameter
}

transformed parameters {
  vector[N_wave] tau;
  tau = gp_matern32(w, gp_time_mu, gp_time_scale, gp_time_lenscale, 1e-3); // Gaussian process

  vector[N_repeat] rho;
  rho[1] = 0;
  rho[2:N_repeat] = inv_quad(r, alpha_repeat, beta_repeat, gamma_repeat); // Gaussian process

  vector[N_part] phi = (phi_loc_hyper + phi_aux) * phi_scale_hyper;

  // Linear predictor
  vector[N] log_lambda; // log rate
  log_lambda = alpha + X * beta + tau[w_idx] + phi[part_idx] .* log_y_lag + rho[r_idx];
}

model {
  /* ===== Model Priors ===== */
  target += normal_lpdf(alpha | 0, 10);
  target += normal_lpdf(beta | 0, 1);

  // Auto-regressive parameter for each participant
  target += normal_lpdf(phi_loc_hyper | 0, 10);
  target += normal_lpdf(phi_scale_hyper | 0, 1);
  target += normal_lpdf(phi_aux | 0, 1);

  // Gaussian process priors
  target += normal_lpdf(gp_time_mu | 0, 1);
  target += gamma_lpdf(gp_time_scale | 5, 5);
  target += gamma_lpdf(gp_time_lenscale | 5, 5);

  target += normal_lpdf(alpha_repeat | 0, 1);
  target += normal_lpdf(beta_repeat | 0, 1);
  target += normal_lpdf(gamma_repeat | 0, 1);

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
