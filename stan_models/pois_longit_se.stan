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

  vector squared_exp(vector r, real gamma, real sigma) {
    return -gamma * (1 - exp(- r.^2 / sigma^2));
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
  array[N] int w_idx;       // Wave variable
  array[N] int r_idx;       // Repeat variable (may be transformed in a non-linear fashion)
  vector[N] y_lag;       // lagged response variable (AR1)

  vector[N_wave] w;     // A array of the number of waves
  vector[N_repeat-1] r;     // A array of the number of repeats

  array[N] int y;           // response variable
}

transformed data {
  array[N_wave] real w_std = to_array_1d((w - mean(w)) / sd(w));
  
  vector[N_repeat-1] r_std = r / max(r);
}

parameters {
  real alpha;             // intercept
  vector[P] beta;         // dummy coefficients
  vector[N_part] phi;     // Auto-regressive parameter for participant

  vector[N_wave] gp_time_mu;      // GP mean
  real<lower=0> gp_time_scale;    // GP scale
  real<lower=0> gp_time_lenscale; // GP lengthscale

  // Repeat effect
  real<lower=0> gamma_repeat;
  real<lower=0> sigma_repeat;
}

transformed parameters {
  // Temporal effect
  vector[N_wave] tau;
  tau = gp_matern32(w_std, gp_time_mu, gp_time_scale, gp_time_lenscale, 1e-3); // Matern 3/2 Gaussian process

  // Repeat effect
  vector[N_repeat] rho;
  rho[1] = 0;
  rho[2:N_repeat] = squared_exp(r_std, gamma_repeat, sigma_repeat);

  // Linear predictor
  vector[N] log_lambda;
  log_lambda = alpha + X * beta + tau[w_idx] + phi[part_idx] .* y_lag + rho[r_idx];
}

model {
  // priors
  target += normal_lpdf(alpha | 0, 10);
  target += normal_lpdf(beta | 0, 1);
  target += normal_lpdf(phi | 0, 1);

  // Temporal effect GP
  target += normal_lpdf(gp_time_mu | 0, 1);
  target += inv_gamma_lpdf(gp_time_scale | 5, 5);
  target += inv_gamma_lpdf(gp_time_lenscale | 5, 5);

  // Repeat effect
  target += inv_gamma_lpdf(gamma_repeat | 1, 1);
  target += inv_gamma_lpdf(sigma_repeat | 1, 1);

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
