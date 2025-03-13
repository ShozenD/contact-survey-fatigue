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
  int<lower=1> N;  // number of observations
  array[N] int y;  // response variable

  int<lower=1> Pu;  // number of fixed effect predictors
  matrix[N, Pu] U;  // fixed effect predictors

  int<lower=1> Pv;
  matrix[N, Pv] V;

  int<lower=1> Pw;  // number of variables for variable selection
  matrix[N, Pw] W;  // variables for variable selection

  real beta0;
  vector[Pu] alpha;
  vector[Pv] beta;
}

parameters {
  // Negative binomial
  real<lower=0> inv_varphi;

  // Horseshoe prior
  vector<upper=0>[Pw] zw;        // Horseshoe auxiliary random variables
  vector<lower=0>[Pw] hs_local;  // Horseshoe local parameters
  real<lower=0> hs_global;       // Global shrinkage parameters
  real<lower=0> hs_slab;         // Slab regularization parameter
}

transformed parameters {
  vector[Pw] gamma = horseshoe(zw, hs_local, hs_global, hs_slab);
  vector[N] log_lambda = beta0 + U*alpha + V*beta + W*gamma;
}

model {
  // Priors
  target += exponential_lpdf(inv_varphi | 1);
  target += normal_lpdf(zw | 0, 1.0/(1.0 - 2/pi()));
  target += student_t_lpdf(hs_local | 3, 0, 1) - rows(hs_local) * log(0.5);
  target += student_t_lpdf(hs_global | 4, 0, (Pw - 1.0)/sqrt(N));
  target += inv_gamma_lpdf(hs_slab | 1, 2);

  // Likelihood
  target += neg_binomial_2_log_lpmf(y | log_lambda, 1/inv_varphi);
}

generated quantities {
  array[N] int y_rep;
  array[N] real log_lik;
  for (n in 1:N) {
    y_rep[n] = neg_binomial_2_log_rng(log_lambda[n], 1/inv_varphi);
    log_lik[n] = neg_binomial_2_log_lpmf(y[n] | log_lambda[n], 1/inv_varphi);
  }
}

