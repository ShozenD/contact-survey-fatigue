functions {
  /* Gaussian process with Matern 3/2 covariance functions */
  vector gp_matern32(array[] real x, vector mu, real scale, real lenscale) {
    int N = num_elements(x);
    matrix[N, N] K = gp_matern32_cov(x, scale, lenscale);
    K = K + diag_matrix(rep_vector(square(1e-3), N));
    matrix[N, N] L = cholesky_decompose(K);

    return L * mu;
  }
}

data {
  int<lower=0> N;

  int<lower=0> P_age;
  int<lower=0> P_hh;
  int<lower=0> P_gender;
  int<lower=0> P_job;
  int<lower=0> P_urbn;
  
  matrix[N, P_age] X_age;
  matrix[N, P_hh] X_hh;
  matrix[N, P_gender] X_gender;
  matrix[N, P_job] X_job;
  matrix[N, P_urbn] X_urbn;

  array[N] int<lower=1> wid; // Wave index
  array[N] int<lower=1> rid; // Repeat index
  array[N] int<lower=0> y;   // Contact counts
}

transformed data{
  int<lower=1> W = max(wid);     // number of waves
  vector[W] w = linspaced_vector(W, 1, W);
  array[W] real wstd = to_array_1d((w - mean(w)) / sd(w));
}

parameters {
  real beta0;                   // intercept

  vector[P_age] beta_age;
  vector[P_hh] beta_hh;
  vector[P_gender] beta_gender;
  vector[P_job] beta_job;
  vector[P_urbn] beta_urbn;

  real<lower=0> inv_varphi; // Reciprocal of the dispersion parameter

  // Time varying coefficients
  vector[W] gp_time_mu;
  real<lower=0> gp_time_scale;
  real<lower=0> gp_time_lenscale;
}

transformed parameters {
  vector[W] tau = gp_matern32(wstd, gp_time_mu, gp_time_scale, gp_time_lenscale);

  vector[N] log_lambda = beta0
                      // Fixed effects
                      + X_age*beta_age
                      + X_hh*beta_hh
                      + X_gender*beta_gender
                      + X_job*beta_job
                      + X_urbn*beta_urbn
                      // Time effect
                      + tau[wid];
}

model {
  target += normal_lupdf(beta0 | 0, 5)
          // Fixed effects priors
          + normal_lupdf(beta_age | 0, 1)
          + normal_lupdf(beta_hh | 0, 1)
          + normal_lupdf(beta_gender | 0, 1)
          + normal_lupdf(beta_job | 0, 1)
          + normal_lupdf(beta_urbn | 0, 1)
          + exponential_lupdf(inv_varphi | 1)
          // Gaussian process prior
          + normal_lupdf(gp_time_mu | 0, 1)
          + inv_gamma_lupdf(gp_time_scale | 5, 5)
          + inv_gamma_lupdf(gp_time_lenscale | 5, 5)
          // likelihood
          + neg_binomial_2_log_lupmf(y | log_lambda, 1.0/inv_varphi);
}

generated quantities {
  array[N] int y_rep;   // posterior predictions
  array[N] real log_lik; // log likelihood

  for (n in 1:N) {
    y_rep[n] = neg_binomial_2_log_rng(log_lambda[n], 1.0/inv_varphi);
    log_lik[n] = neg_binomial_2_log_lpmf(y[n] | log_lambda[n], 1.0/inv_varphi);
  }
}
