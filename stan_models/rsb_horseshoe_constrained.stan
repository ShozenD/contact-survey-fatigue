functions {
  /* Efficient computation of the horseshoe prior
   * see Appendix C.1 in https://projecteuclid.org/euclid.ejs/1513306866
   * Args:
   *   z: standardized population-level coefficients
   *   lambda: local shrinkage parameters
   *   tau: global shrinkage parameter
   *   c2: slab regularization parameter
   * Returns:
   *   population-level coefficients following the horseshoe prior
   */
  vector horseshoe(vector z, vector lambda, real tau, real c2) {
    int K = rows(z);
    vector[K] lambda2 = square(lambda);
    vector[K] lambda_tilde = sqrt(c2 * lambda2 ./ (c2 + tau^2 * lambda2));
    return z .* lambda_tilde * tau;
  }

  real rsb_lpdf(vector x, real alpha, real beta) {
    return sum(
      -beta(alpha, beta) + (alpha - 1)*log(log(1 + x)) - log(1 + x) - (alpha + beta)*log(1 + log(1 + x))
    );
  }

  real rsb_mixture_lpdf(vector x, real theta, real alpha, real beta) {
    return log_sum_exp(log1m(theta) + normal_lpdf(x | 1, 0.01), log(theta) + rsb_lpdf(x | alpha, beta));
  }
}

data {
  /* ===== Sample size ===== */
  int<lower=1> N0; // No. of first-time participants
  int<lower=1> N1; // No. of repeating participants

  /* ===== Data ===== */
  int<lower=1> P;  // Number of parameters
  matrix[N0,P] X0; // Design matrix
  matrix[N1,P] X1; // Design matrix

  /* ===== Outcomes ===== */
  array[N0] int y0; // Array of outcomes (first-time)
  array[N1] int y1; // Array of outcomes (repeating)

  /* ===== Horseshoe prior ===== */
  int<lower=0> p0; // No. of non-zero coefficients
  real<lower=0> hs_df;  // local degrees of freedom
  real<lower=0> hs_df_global;  // global degrees of freedom
  real<lower=0> hs_df_slab;  // slab degrees of freedom
  real<lower=0> hs_scale_slab;  // slab prior scale
}

transformed data {
  int N = N0 + N1;
  array[N] int y = append_array(y0, y1);

  // Scale for the global shrinkage parameter
  real<lower=0> hs_scale_global = p0 * 1.0 /(P - p0) * 1/sqrt(N);
}

parameters {
  real alpha;

  real<lower=0, upper=1> theta;
  vector<lower=0>[N] eta;

  /* ===== Horseshoe priors ===== */
  // === Coefficients (Baseline) ====
  vector[P] zb;        // Auxiliary variables
  vector<lower=0>[P] hs_local;  // local shrinkage parameters
  real<lower=0> hs_global;      // global shrinkage parameters
  real<lower=0> hs_slab;        // slab regularization parameter

  // === Coefficients (Repeat) ====
  vector<lower=0>[P] zb1;        // Auxiliary variables
  vector<lower=0>[P] hs_local1;  // local shrinkage parameters
  real<lower=0> hs_global1;      // global shrinkage parameters
  real<lower=0> hs_slab1;        // slab regularization parameter
}

transformed parameters {
  // === Coefficients (Baseline) ====
  vector[P] beta0;
  beta0 = horseshoe(zb, hs_local, hs_global, hs_scale_slab^2 * hs_slab);  // compute actual regression coefficients

  // === Coefficients (Repeat) ====
  vector<lower=0>[P] beta1;
  beta1 = horseshoe(zb1, hs_local1, hs_global1, hs_scale_slab^2 * hs_slab1);  // compute actual regression coefficients
}

model {
  // Priors
  target += normal_lpdf(alpha | 0, 10);

  target += beta_lpdf(theta | 0.5, 0.5);
  target += rsb_mixture_lpdf(eta | theta, 0.5, 0.5);

  /* ===== Horseshoe priors ===== */
  // === Coefficients (Baseline) ====
  target += std_normal_lpdf(zb);
  target += student_t_lpdf(hs_local | hs_df, 0, 1) - rows(hs_local) * log(0.5);
  target += student_t_lpdf(hs_global | hs_df_global, 0, hs_scale_global) - 1 * log(0.5);
  target += inv_gamma_lpdf(hs_slab | 0.5 * hs_df_slab, 0.5 * hs_df_slab);

  // === Coefficients (Repeat) ====
  target += std_normal_lpdf(zb1);
  target += student_t_lpdf(hs_local1 | hs_df, 0, 1) - rows(hs_local1) * log(0.5);
  target += student_t_lpdf(hs_global1 | hs_df_global, 0, hs_scale_global) - 1 * log(0.5);
  target += inv_gamma_lpdf(hs_slab1 | 0.5 * hs_df_slab, 0.5 * hs_df_slab);

  // ===== Likelihood =====
  {
    vector[N0] log_lambda0 = X0*beta0;
    vector[N1] log_lambda1 = X1*beta0 - X1*beta1;
    vector[N] log_lambda = alpha + append_row(log_lambda0, log_lambda1) + log(eta);
    target += poisson_log_lpmf(y | log_lambda); // Update log-likelihood
  }
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;

  {
    vector[N0] log_lambda0 = X0*beta0;
    vector[N1] log_lambda1 = X1*beta0 - X1*beta1;
    vector[N] log_lambda = alpha + append_row(log_lambda0, log_lambda1) + log(eta);

    for (i in 1:N) {
      log_lik[i] = poisson_log_lpmf(y | log_lambda[i]);
    }

    yhat = poisson_rng(exp(log_lambda));
  }
}
