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
  vector logistic(vector x, real alpha, real beta) {
    return exp(alpha) * x.^beta ./ (1 + exp(alpha) * x.^beta);
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

  vector[N_wave] w;     // A array of the number of waves
  vector[N_repeat-1] r; // A array of the number of repeats

  array[N] int y;           // response variable
}

transformed data{
  // Standardize wave variable
  array[N_wave] real w_std = to_array_1d((w - mean(w)) / sd(w));

  // Standardize repeat variable
  vector[N_repeat-1] r_norm = r / max(r);
}

parameters {
  real alpha;                // Intercept
  vector[P] beta;            // dummy coefficients
  real<lower=0> reciprocal_phi; // Reciprocal of the dispersion parameter

  // Time varying coefficients
  vector[N_wave] gp_time_mu;
  real<lower=0> gp_time_scale;
  real<lower=0> gp_time_lenscale;
}

transformed parameters {
  vector[N_wave] tau;
  tau = gp_matern32(w_std, gp_time_mu, gp_time_scale, gp_time_lenscale, 1e-3); // Gaussian process

  // Linear predictor
  vector[N] log_lambda; // log rate
  log_lambda = alpha + X*beta + tau[w_idx];
}

model {
  /* ===== Model Priors ===== */
  target += normal_lpdf(alpha | 0, 10);
  target += normal_lpdf(beta | 0, 1);
  target += exponential_lpdf(reciprocal_phi | 1);

  // Auto-regressive parameter for each participant
  // target += normal_lpdf(phi | 0, 1);

  // Gaussian process priors
  target += normal_lpdf(gp_time_mu | 0, 1);
  target += gamma_lpdf(gp_time_scale | 5, 5);
  target += gamma_lpdf(gp_time_lenscale | 5, 1);
  
  // likelihood
  target += neg_binomial_2_lpmf(y | exp(log_lambda), 1.0/reciprocal_phi);
}

generated quantities {
  array[N] int yhat;   // posterior predictions
  array[N] real log_lik; // log likelihood

  for (n in 1:N) {
    yhat[n] = neg_binomial_2_rng(exp(log_lambda[n]), 1.0/reciprocal_phi);
    log_lik[n] = neg_binomial_2_lpmf(y[n] | exp(log_lambda[n]), 1.0/reciprocal_phi);
  }
}
