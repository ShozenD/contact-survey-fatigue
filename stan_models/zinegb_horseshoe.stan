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
  int<lower=1> N;
  int<lower=1> Nrep;
  int<lower=1> Nzero;

  /* ===== Design matrices ===== */
  int<lower=1> P;     // Number of parameters
  int<lower=1> Prep;
  matrix[N,P] X;
  matrix[Nrep,Prep] Xrep;

  /* ===== Outcomes ===== */
  array[N] int y;     // Array of observed counts

  /* ===== Indices ===== */
  array[Nrep] int<lower=1> ridx;     // Index of the repeating participants
  array[Nzero] int<lower=1> zidx;    // Index of zero counts
  array[N-Nzero] int<lower=1> nzidx; // Index of non-zero counts

  /* ===== Horseshoe prior ===== */
  int<lower=0> p0;    // No. of non-zero coefficients
  int<lower=0> p0rep; // No. of non-zero coefficients for repeat effects
  real<lower=0> hs_df;  // local degrees of freedom
  real<lower=0> hs_df_global;  // global degrees of freedom
  real<lower=0> hs_df_slab;  // slab degrees of freedom
  real<lower=0> hs_scale_slab;  // slab prior scale
}

transformed data {
  // Scale for the global shrinkage parameter
  real<lower=0> hs_scale_global = p0 * 1.0/(2*P - p0) * 1.0/sqrt(N);
  real<lower=0> hs_scale_global_rep = p0rep * 1.0/(2*Prep - p0rep) * 1.0/sqrt(Nrep);
}

parameters {
  real alpha; // Global baseline parameter
  real tau;   // Constant connecting the log-link and the logit-link
  real<lower=0> inverse_phi; // Inverse overdispersion parameter

  /* ===== Fixed effects ===== */
  // Horseshoe prior
  vector[P] zb;                 // Horseshoe auxiliary random variables
  vector<lower=0>[P] hs_local;  // Horseshoe local parameters
  real<lower=0> hs_global;      // Global shrinkage parameters
  real<lower=0> hs_slab;        // Slab regularization parameter

  vector<lower=0>[Prep] rzb;           // Horseshoe auxiliary random variables (repeat effects)
  vector<lower=0>[Prep] hs_local_rep;  // Horseshoe local parameters
  real<lower=0> hs_global_rep;         // Global shrinkage parameters
  real<lower=0> hs_slab_rep;           // Slab regularization parameter
}

transformed parameters {
  /* ===== Fixed effects ===== */
  vector[P] beta;
  vector[Prep] gamma;
  beta = horseshoe(zb, hs_local, hs_global, hs_scale_slab^2 * hs_slab);
  gamma = horseshoe(rzb, hs_local_rep, hs_global_rep, hs_scale_slab^2 * hs_slab_rep);
}

model {
  // Priors
  target += normal_lpdf(alpha | 0, 10);
  target += normal_lpdf(tau | 0, 5);
  target += exponential_lpdf(inverse_phi | 1);

  /* ===== Horseshoe priors ===== */
  target += std_normal_lpdf(zb);    // Fixed effects
  target += student_t_lpdf(hs_local | hs_df, 0, 1) - rows(hs_local) * log(0.5);
  target += student_t_lpdf(hs_global | hs_df_global, 0, hs_scale_global) - 1 * log(0.5);
  target += inv_gamma_lpdf(hs_slab | 0.5 * hs_df_slab, 0.5 * hs_df_slab);

  target += normal_lpdf(rzb | 0, 1.0/(1.0 - 2.0/pi())); // Repeat effects
  target += student_t_lpdf(hs_local_rep | hs_df, 0, 1) - rows(hs_local_rep) * log(0.5);
  target += student_t_lpdf(hs_global_rep | hs_df_global, 0, hs_scale_global) - 1 * log(0.5);
  target += inv_gamma_lpdf(hs_slab_rep | 0.5 * hs_df_slab, 0.5 * hs_df_slab);

  // ===== Likelihood =====
  {
    vector[N] log_lambda = alpha + X*beta;
    log_lambda[ridx] = log_lambda[ridx] - Xrep*gamma;
    vector[N] theta = inv_logit(-tau*log_lambda);

    // Update log-likelihood
    for (i in 1:Nzero) {
      target += log_mix(theta[zidx[i]], 0, neg_binomial_2_log_lpmf(y[zidx[i]] | log_lambda[zidx[i]], 1/inverse_phi)); // zero case
    }
    for (i in 1:(N-Nzero)) {
      target += log1m(theta[nzidx[i]]) + neg_binomial_2_log_lpmf(y[nzidx[i]] | log_lambda[nzidx[i]], 1/inverse_phi);  // non-zero case
    }
  }
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;

  { // local scope
    vector[N] log_lambda = alpha + X*beta;
    log_lambda[ridx] = log_lambda[ridx] - Xrep*gamma;
    vector[N] theta = inv_logit(-tau*log_lambda);

    for (i in 1:Nzero) {
      log_lik[zidx[i]] += log_mix(theta[zidx[i]], 0, neg_binomial_2_log_lpmf(y[zidx[i]] | log_lambda[zidx[i]], 1/inverse_phi)); // zero case
    }
    for (i in 1:(N-Nzero)) {
      log_lik[nzidx[i]] += log1m(theta[nzidx[i]]) + neg_binomial_2_log_lpmf(y[nzidx[i]] | log_lambda[nzidx[i]], 1/inverse_phi);  // non-zero case
    }

    for (i in 1:N) {
      real z = bernoulli_rng(1 - theta[i]);
      if (z > 0) {
        yhat[i] = neg_binomial_2_log_rng(log_lambda[i], 1/inverse_phi);
      } else {
        yhat[i] = 0;
      }
    }
  }
}
