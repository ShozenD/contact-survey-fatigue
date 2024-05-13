functions {
  // An function that appends four intger arrays
  array[] int append_array_int4(array[] int a1, array[] int a2, array[] int a3, array[] int a4) {
    return append_array(a1, append_array(a2, append_array(a3, a4)));
  }

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
}

data {
  /* ===== Sample size ===== */
  int<lower=1> N1; // No. of first-time participants
  int<lower=1> N2; // No. of repeating participants

  /* ===== Design matrices ===== */
  int<lower=1> P;   // Number of parameters
  matrix[N1,P] X1;  // Design matrix
  matrix[N2,P] X2;  // Design matrix

  /* ===== Outcomes ===== */
  array[N1] int y1; // Array of outcomes (first-time 0 contacts)
  array[N2] int y2; // Array of outcomes (repeating 0 contacts)

  /* ===== Horseshoe prior ===== */
  int<lower=0> p0; // No. of non-zero coefficients
  real<lower=0> hs_df;  // local degrees of freedom
  real<lower=0> hs_df_global;  // global degrees of freedom
  real<lower=0> hs_df_slab;  // slab degrees of freedom
  real<lower=0> hs_scale_slab;  // slab prior scale
}

transformed data {
  int N = N1 + N2;
  array[N] int y = append_array(y1, y2);

  // Scale for the global shrinkage parameter
  real<lower=0> hs_scale_global = p0 * 1.0/(2*P - p0) * 1.0/sqrt(N);
}

parameters {
  real alpha; // Global baseline parameter
  real<lower=0> inverse_phi;

  /* ===== Fixed effects ===== */
  // Horseshoe prior
  vector[P] zb;                 // Horseshoe auxiliary random variables
  vector<lower=0>[P] zb_rp;     // Horseshoe auxiliary random variables (repeat effects)
  vector<lower=0>[2*P] hs_local;  // Horseshoe local parameters
  real<lower=0> hs_global;      // Global shrinkage parameters
  real<lower=0> hs_slab;        // Slab regularization parameter
}

transformed parameters {
  /* ===== Fixed effects ===== */
  vector[2*P] beta;
  beta = horseshoe(append_row(zb, zb_rp),
                   hs_local,
                   hs_global,
                   hs_scale_slab^2 * hs_slab);
}

model {
  // Priors
  target += normal_lpdf(alpha | 0, 10);
  target += exponential_lpdf(inverse_phi | 1);

  /* ===== Horseshoe priors ===== */
  target += std_normal_lpdf(zb);    // Fixed effects
  target += normal_lpdf(zb_rp | 0, 1.0/(1.0 - 2.0/pi())); // Repeat effects
  target += student_t_lpdf(hs_local | hs_df, 0, 1) - rows(hs_local) * log(0.5);
  target += student_t_lpdf(hs_global | hs_df_global, 0, hs_scale_global) - 1 * log(0.5);
  target += inv_gamma_lpdf(hs_slab | 0.5 * hs_df_slab, 0.5 * hs_df_slab);

  // ===== Likelihood =====
  {
    vector[N1] log_lambda1 = alpha + X1*beta[1:P];
    vector[N2] log_lambda2 = alpha + X2*beta[1:P] - X2*beta[(P+1):(2*P)];

    vector[N] log_lambda = append_row(log_lambda1, log_lambda2);
    // Update log-likelihood
    target += neg_binomial_2_lpmf(y | exp(log_lambda), 1/inverse_phi);
  }
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;

  {
    vector[N1] log_lambda1 = alpha + X1*beta[1:P];
    vector[N2] log_lambda2 = alpha + X2*beta[1:P] - X2*beta[(P+1):(2*P)];
    vector[N] log_lambda = append_row(log_lambda1, log_lambda2);

    for (i in 1:N) {
      yhat[i] = neg_binomial_2_rng(exp(log_lambda[i]), 1/inverse_phi);
      log_lik[i] = neg_binomial_2_lpmf(y | exp(log_lambda[i]), 1/inverse_phi);
    }
  }
}
