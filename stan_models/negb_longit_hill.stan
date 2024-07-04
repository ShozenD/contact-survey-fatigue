functions {
  /* Gaussian process with Matern 3/2 covariance functions */
  vector gp_matern32(array[] real x, vector mu, real scale, real lenscale, real nugget) {
    int N = num_elements(x);
    matrix[N, N] K = gp_matern32_cov(x, scale, lenscale);
    K = K + diag_matrix(rep_vector(square(nugget), N));
    matrix[N, N] L = cholesky_decompose(K);

    return L * mu;
  }

  /* Logistic function */
  vector hill(vector x, real gamma, real zeta, real eta) {
    return -gamma * exp(zeta) * x.^eta ./ (1 + exp(zeta) * x.^eta);
  }
}

data {
  int<lower=1> N;            // number of observations
  int<lower=1> P;            // number of fixed effects
  int<lower=1> Q;            // number of random effects
  matrix[N, P] X;            // fixed effects design matrix
  matrix[N, Q] Z;            // random effects design matrix
  array[N] int<lower=1> wid; // Wave index
  array[N] int<lower=1> rid; // Repeat index
  array[N] int<lower=1> y;   // Contact counts
}

transformed data{
  int<lower=1> W = max(wid);     // Number of waves
  vector[W] w = linspaced_vector(W, 1, w)
  array[W] real wstd = to_array_1d((w - mean(w)) / sd(w));

  int<lower=1> R = max(rid);     // Number of repeats
  vector[R] r = linspaced_vector(R, 0, R - 1);
}

parameters {
  real alpha;                       // Intercept
  vector[P] beta_fixed;             // Fixed effect coefficients
  real<lower=0> sigma_beta_random;  // Hierarchical variance for the coefficients
  vector[Q] beta_random;            // Random effects
  real<lower=0> reciprocal_phi;     // Reciprocal of the over-dispersion parameter

  // Gaussian process parameters
  vector[N_wave] gp_time_mu;
  real<lower=0> gp_time_scale;
  real<lower=0> gp_time_lenscale;

  // Hill function parameters
  real<lower=0> gamma;
  real zeta;
  real<lower=0> eta;
}

transformed parameters {
  vector[W] tau = gp_matern32(wstd, gp_time_mu, gp_time_scale, gp_time_lenscale, 1e-3); // Gaussian process
  vector[N] log_lambda = alpha + X*beta_fixed + Z*beta_random + tau[wid] + hill(rid - 1, gamma, zeta, eta); // Linear predictor
}

model {
  /* ===== Model Priors ===== */
  target += normal_lupdf(alpha | 0, 10);
  target += normal_lupdf(beta_fixed | 0, 1);
  target += cauchy_lupdf(sigma_beta_random | 0, 1);
  target += normal_lupdf(beta_random | 0, sigma_beta_random);
  target += exponential_lupdf(reciprocal_phi | 1);

  // Gaussian process priors
  target += normal_lupdf(gp_time_mu | 0, 1);
  target += inv_gamma_lupdf(gp_time_scale | 5, 1);
  target += inv_gamma_lupdf(gp_time_lenscale | 5, 1);

  // Hill function priors
  target += normal_lupdf(gamma | 0, 1);
  target += normal_lupdf(zeta | 0, 1);
  target += exponential_lupdf(eta | 1);
  
  // likelihood
  target += neg_binomial_2_log_lupmf(y | log_lambda, 1.0/reciprocal_phi);
}

generated quantities {
  array[N] int yhat;   // posterior predictions
  array[N] real log_lik; // log likelihood
  vector[R] rho = hill(r, gamma, zeta, eta); // Hill function

  for (n in 1:N) {
    yhat[n] = neg_binomial_2_log_rng(log_lambda[n], 1.0/reciprocal_phi);
    log_lik[n] = neg_binomial_2_log_lpmf(y[n] | log_lambda[n], 1.0/reciprocal_phi);
  }
}
