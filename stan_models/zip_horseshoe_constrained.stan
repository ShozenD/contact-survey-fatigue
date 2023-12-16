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
  int<lower=1> N00;   // No. of first-time participants with zero contacts
  int<lower=1> N10;   // No. of repeating participants with zero contacts
  int<lower=1> N01;   // No. of first-time participants with 1 or more contacts
  int<lower=1> N11;   // No. of repeating participants with 1 or more contacts

  /* ===== Design matrices ===== */
  int<lower=1> P;     // Number of parameters
  matrix[N00,P] X00;  // Design matrix
  matrix[N10,P] X10;  // Design matrix
  matrix[N01,P] X01;  // Design matrix
  matrix[N11,P] X11;  // Design matrix

  /* ===== Outcomes ===== */
  array[N00] int y00; // Array of outcomes (first-time 0 contacts)
  array[N10] int y10; // Array of outcomes (repeating 0 contacts)
  array[N01] int y01; // Array of outcomes (first-time 1 or more contacts)
  array[N11] int y11; // Array of outcomes (repeating 1 or more contacts)

  /* ===== Horseshoe prior ===== */
  int<lower=0> p0; // No. of non-zero coefficients
  real<lower=0> hs_df;  // local degrees of freedom
  real<lower=0> hs_df_global;  // global degrees of freedom
  real<lower=0> hs_df_slab;  // slab degrees of freedom
  real<lower=0> hs_scale_slab;  // slab prior scale
}

transformed data {
  int N = N00 + N10 + N01 + N11;
  array[N] int y = append_array_int4(y00, y10, y01, y11);

  // Scale for the global shrinkage parameter
  real<lower=0> hs_scale_global = p0 * 1.0 /(P - p0) * 1/sqrt(N);
}

parameters {
  real alpha; // Global baseline
  vector[P] beta0; // Baseline coefficients
  real tau; // Constant connecting the log-link and the logit-link

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
  target += normal_lpdf(alpha | 0, 10);
  target += normal_lpdf(beta0 | 0, 1);
  target += normal_lpdf(tau | 0, 5);

  // Horseshoe priors
  target += std_normal_lpdf(zb);
  target += student_t_lpdf(hs_local | hs_df, 0, 1) - rows(hs_local) * log(0.5);
  target += student_t_lpdf(hs_global | hs_df_global, 0, hs_scale_global) - 1 * log(0.5);
  target += inv_gamma_lpdf(hs_slab | 0.5 * hs_df_slab, 0.5 * hs_df_slab);

  {
    vector[N00] log_lambda00 = alpha + X00*beta0;
    vector[N10] log_lambda10 = alpha + X10*beta0 - X10*beta1;
    vector[N01] log_lambda01 = alpha + X01*beta0;
    vector[N11] log_lambda11 = alpha + X11*beta0 - X11*beta1;

    vector[N00] theta00 = inv_logit(-tau*log_lambda00);
    vector[N10] theta10 = inv_logit(-tau*log_lambda10);
    vector[N01] theta01 = inv_logit(-tau*log_lambda01);
    vector[N11] theta11 = inv_logit(-tau*log_lambda11);

    // Update log-likelihood
    // Zero case
    for (i in 1:N00) {
      target += log_mix(theta00[i], 0, poisson_lpmf(y00[i] | exp(log_lambda00[i])));
      // target += log_sum_exp(log(theta00[i]), log1m(theta00[i]) + poisson_lpmf(y00[i] | exp(log_lambda00[i])));
    }
    for (i in 1:N10) {
      target += log_mix(theta10[i], 0, poisson_lpmf(y10[i] | exp(log_lambda10[i])));
      // target += log_sum_exp(log(theta10), log1m(theta10) + poisson_lpmf(y10 | exp(log_lambda10)));
    }

    // Non-zero case
    target += log1m(theta01);
    target += log1m(theta11);
    target += poisson_lpmf(y01 | exp(log_lambda01));
    target += poisson_lpmf(y11 | exp(log_lambda11));
  }
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;

  {
    vector[N00] log_lambda00 = alpha + X00*beta0;
    vector[N10] log_lambda10 = alpha + X10*beta0 - X10*beta1;
    vector[N01] log_lambda01 = alpha + X01*beta0;
    vector[N11] log_lambda11 = alpha + X11*beta0 - X11*beta1;
    vector[N] log_lambda = append_row(log_lambda00, append_row(log_lambda10, append_row(log_lambda01, log_lambda11)));

    vector[N00] theta00 = inv_logit(-tau*log_lambda00);
    vector[N10] theta10 = inv_logit(-tau*log_lambda10);
    vector[N01] theta01 = inv_logit(-tau*log_lambda01);
    vector[N11] theta11 = inv_logit(-tau*log_lambda11);
    vector[N] theta = append_row(theta00, append_row(theta10, append_row(theta01, theta11)));

    for (i in 1:N) {
      if (i <= N00) {
        log_lik[i] = log_mix(theta[i], 0, poisson_lpmf(y[i] | exp(log_lambda[i])));
      } else if (i <= N00 + N10) {
        log_lik[i] = log_mix(theta[i], 0, poisson_lpmf(y[i] | exp(log_lambda[i])));
      } else if (i <= N00 + N10 + N01) {
        log_lik[i] = log1m(theta[i]) + poisson_lpmf(y[i] | exp(log_lambda[i]));
      } else {
        log_lik[i] = log1m(theta[i]) + poisson_lpmf(y[i] | exp(log_lambda[i]));
      }

      real z = bernoulli_rng(1-theta[i]);
      if (z > 0) {
        yhat[i] = poisson_rng(exp(log_lambda[i]));
      } else {
        yhat[i] = 0;
      }
    }
  }
}
