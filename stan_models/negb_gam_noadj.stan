functions {
  // ===== HSGP functions =====
  vector diagSPD_SE(real alpha, real rho, real L, int M) {
    return alpha * sqrt(sqrt(2*pi()) * rho) * exp(-0.25*(rho*pi()/2/L)^2 * linspaced_vector(M, 1, M)^2);
  }

  // Basis functions
  matrix PHI(vector x, real L, int M) {
    return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L);
  }

  // Hilbert Space approximate GPs
  vector hsgp(vector z, matrix phi, real sigma, real lenscale, real L) {
    int M = rows(z);
    return diag_post_multiply(phi, diagSPD_SE(sigma, lenscale, L, M))*z;
  }
}

data {
  int<lower=1> N;              // Number of participants
  int<lower=1> A;              // Number of age inputs
  int<lower=1> P;              // The number of participant covariates
  int<lower=1> Q;              // The number of jobs with repeat effects

  matrix[N, P] X;              // Fixed design matrix
  matrix[N, Q] Z;              // Repeat effect design matrix
  array[N] int<lower=1> aid;   // age index
  array[N] int<lower=1> rid;   // repeat index

  real<lower=0> hatGamma;
  real hatZeta;
  real<lower=0> hatEta;

  int<lower=1> M;
  real<lower=0> C;
  vector[A] x_hsgp;

  array[N] int<lower=0> y; // Array of contact reports
}

transformed data {
  // ========== HSGP ==========
  real L = C * max(x_hsgp);
  matrix[A,M] phi = PHI(x_hsgp, L, M); // Basis functions
}

parameters {
  real alpha;     // Baseline parameter
  vector[P] beta; // Participant covariate parameters
  real<lower=0> reciprocal_phi; // Reciprocal of the dispersion parameter

  // ========== HSGP ==========
  real<lower=0> lenscale; // GP lengthscale
  real<lower=0> sigma;    // GP magnitude
  vector[M] zb;
}

transformed parameters {
  vector[A] log_m = alpha + hsgp(zb, phi, sigma, lenscale, L);
  vector[N] log_lambda = log_m[aid] + X*beta;
}

model {
  real lp = 0;

  // ========== Priors ==========
  lp = lp + normal_lupdf(alpha | 0, 10);
  lp = lp + normal_lupdf(beta | 0, 1);
  lp = lp + exponential_lupdf(reciprocal_phi | 1);

  // ========== HSGP ==========
  lp = lp + inv_gamma_lupdf(lenscale | 5, 1);
  lp = lp + inv_gamma_lupdf(sigma | 5, 1);
  lp = lp + normal_lupdf(zb | 0, 1);

  // ========== Likelihood ==========
  target += lp + neg_binomial_2_log_lupmf(y | log_lambda, 1.0/reciprocal_phi);
}

generated quantities {
  array[N] int y_rep;
  vector[N] log_lik;

  for (i in 1:N) {
    y_rep[i] = neg_binomial_2_log_rng(log_lambda[i], 1.0/reciprocal_phi);
    log_lik[i] = neg_binomial_2_log_lpmf(y[i] | log_lambda[i], 1.0/reciprocal_phi);
  }
}

