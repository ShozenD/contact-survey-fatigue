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
  real<lower=0> hs_scale_global = p0 * 1.0/(2*P - p0) * 1.0/sqrt(N);
}

parameters {
  real alpha; // Global baseline parameter
  real tau;   // Constant connecting the log-link and the logit-link

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
  target += normal_lpdf(tau | 0, 5);

  /* ===== Horseshoe priors ===== */
  target += std_normal_lpdf(zb);    // Fixed effects
  target += normal_lpdf(zb_rp | 0, 1.0/(1.0 - 2/pi)); // Repeat effects
  target += student_t_lpdf(hs_local | hs_df, 0, 1) - rows(hs_local) * log(0.5);
  target += student_t_lpdf(hs_global | hs_df_global, 0, hs_scale_global) - 1 * log(0.5);
  target += inv_gamma_lpdf(hs_slab | 0.5 * hs_df_slab, 0.5 * hs_df_slab);

  // ===== Likelihood =====
  {
    vector[N00] log_lambda00 = alpha + X00*beta[1:P];
    vector[N10] log_lambda10 = alpha + X10*beta[1:P] - X10*beta[(P+1):(2*P)];
    vector[N01] log_lambda01 = alpha + X01*beta[1:P];
    vector[N11] log_lambda11 = alpha + X11*beta[1:P] - X11*beta[(P+1):(2*P)];

    vector[N00] theta00 = inv_logit(-tau*log_lambda00);
    vector[N10] theta10 = inv_logit(-tau*log_lambda10);
    vector[N01] theta01 = inv_logit(-tau*log_lambda01);
    vector[N11] theta11 = inv_logit(-tau*log_lambda11);

    // Update log-likelihood
    // Zero case
    for (i in 1:N00) {
      target += log_mix(theta00[i], 0, poisson_log_lpmf(y00[i] | log_lambda00[i]));
    }
    for (i in 1:N10) {
      target += log_mix(theta10[i], 0, poisson_log_lpmf(y10[i] | log_lambda10[i]));
    }

    // Non-zero case
    target += log1m(theta01) + poisson_log_lpmf(y01 | log_lambda01);
    target += log1m(theta11) + poisson_log_lpmf(y11 | log_lambda11);
  }
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;

  {
    vector[N00] log_lambda00 = alpha + X00*beta[1:P];
    vector[N10] log_lambda10 = alpha + X10*beta[1:P] - X10*beta[(P+1):(2*P)];
    vector[N01] log_lambda01 = alpha + X01*beta[1:P];
    vector[N11] log_lambda11 = alpha + X11*beta[1:P] - X11*beta[(P+1):(2*P)];
    vector[N] log_lambda = append_row(log_lambda00, append_row(log_lambda10, append_row(log_lambda01, log_lambda11)));

    vector[N00] theta00 = inv_logit(-tau*log_lambda00);
    vector[N10] theta10 = inv_logit(-tau*log_lambda10);
    vector[N01] theta01 = inv_logit(-tau*log_lambda01);
    vector[N11] theta11 = inv_logit(-tau*log_lambda11);
    vector[N] theta = append_row(theta00, append_row(theta10, append_row(theta01, theta11)));

    for (i in 1:N) {
      if (i <= N00) {
        log_lik[i] = log_mix(theta[i], 0, poisson_log_lpmf(y[i] | log_lambda[i]));
      } else if (i <= N00 + N10) {
        log_lik[i] = log_mix(theta[i], 0, poisson_log_lpmf(y[i] | log_lambda[i]));
      } else if (i <= N00 + N10 + N01) {
        log_lik[i] = log1m(theta[i]) + poisson_log_lpmf(y[i] | log_lambda[i]);
      } else {
        log_lik[i] = log1m(theta[i]) + poisson_log_lpmf(y[i] | log_lambda[i]);
      }

      real z = bernoulli_rng(1-theta[i]);
      if (z > 0) {
        yhat[i] = poisson_log_rng(log_lambda[i]);
      } else {
        yhat[i] = 0;
      }
    }
  }
}
