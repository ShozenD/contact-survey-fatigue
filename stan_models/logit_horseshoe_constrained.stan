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
}

data {
  int<lower=1> N0; // No. of first-time participants
  int<lower=1> N1; // No. of repeating participants
  int<lower=1> P;  // Number of parameters
  matrix[N0,P] X0; // Design matrix
  matrix[N1,P] X1; // Design matrix
  array[N0] int y0; // Array of outcomes (first-time)
  array[N1] int y1; // Array of outcomes (repeating)

  real<lower=0> hs_df;  // local degrees of freedom
  real<lower=0> hs_df_global;  // global degrees of freedom
  real<lower=0> hs_df_slab;  // slab degrees of freedom
  real<lower=0> hs_scale_global;  // global prior scale
  real<lower=0> hs_scale_slab;  // slab prior scale
}

transformed data {
  int N = N0 + N1;
  array[N] int y = append_array(y0, y1);
}

parameters {
  real alpha;
  vector[P] beta0; // Baseline coefficients

  // local parameters for horseshoe prior
  vector<lower=0>[P] zb;
  vector<lower=0>[P] hs_local;
  // horseshoe shrinkage parameters
  real<lower=0> hs_global;  // global shrinkage parameters
  real<lower=0> hs_slab;  // slab regularization parameter
}

transformed parameters {
  vector<lower=0>[P] beta1;  // population-level effects
  beta1 = horseshoe(zb, hs_local, hs_global, hs_scale_slab^2 * hs_slab);  // compute actual regression coefficients
}

model {
  // Priors
  target += normal_lpdf(alpha | 0, 5);
  target += normal_lpdf(beta0 | 0, 1);

  // Horseshoe priors
  target += std_normal_lpdf(zb);
  target += student_t_lpdf(hs_local | hs_df, 0, 1) - rows(hs_local) * log(0.5);
  target += student_t_lpdf(hs_global | hs_df_global, 0, hs_scale_global) - 1 * log(0.5);
  target += inv_gamma_lpdf(hs_slab | 0.5 * hs_df_slab, 0.5 * hs_df_slab);

  {
    vector[N0] logit_pi0 = X0*beta0;
    vector[N1] logit_pi1 = X1*beta0 - X1*beta1;
    vector[N] logit_pi = alpha + append_row(logit_pi0, logit_pi1);
    target += bernoulli_logit_lpmf(y | logit_pi); // Update log-likelihood
  }
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;

  {
    vector[N0] logit_pi0 = X0*beta0;
    vector[N1] logit_pi1 = X1*beta0 - X1*beta1;
    vector[N] logit_pi = alpha + append_row(logit_pi0, logit_pi1);

    for (i in 1:N) {
      log_lik[i] = bernoulli_logit_lpmf(y[i] | logit_pi[i]);
    }

    yhat = bernoulli_logit_rng(logit_pi);
  }
}
