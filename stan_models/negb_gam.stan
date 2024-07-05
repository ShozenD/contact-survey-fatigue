functions {
  matrix Hill(vector r, vector gamma, vector zeta, vector eta) {
    int Q = rows(gamma);
    int R = rows(r);
    row_vector[R] r = to_row_vector(r);

    matrix[Q, R] rho;
    for (q in 1:Q) {
      rho[q,:] = -gamma[q] * exp(zeta[q]) * r^eta[q] ./ (1 + exp(zeta[q]) * r^eta[q]);
    }

    return rho;
  }

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
  int<lower=1> R = max(rid);
  if (R == 1) {
    vector[1] r;
    r[1] = 0;
  } else {
    vector[R] r = linspaced_vector(R, 0, R-1);
  }

  real L = C * max(x_hsgp);
  matrix[A,M] phi = PHI(x_hsgp, L, M);
}

parameters {
  real alpha;     // Baseline parameter
  vector[P] beta;  // Participant covariate parameters
  real<lower=0> reciprocal_phi; // Reciprocal of the dispersion parameter

  // ========== Repeat effect terms ==========
  vector<lower=0>[Q] gamma;
  vector[Q] zeta;
  vector<lower=0>[Q] eta;

  // ========== HSGP ==========
  real<lower=0> lenscale; // GP lengthscale
  real<lower=0> sigma;    // GP magnitude
  vector[M] zb;
}

transformed parameters {
  vector[A] log_m = alpha + hsgp(zb, phi, sigma, lenscale, L); // Log contact intensity
  matrix[Q, R] rho = Hill(r, gamma, zeta, eta);
  vector[N] log_lambda = log_m[aid] + X*beta;

  for (i in 1:N) {
    log_lambda[i] = log_lambda[i] + Z[i,:] * rho[:,rid[i]];
  }
}

model {
  real lp = 0;

  // ========== Priors ==========
  lp = lp + normal_lupdf(alpha | 0., 10);
  lp = lp + normal_lupdf(beta | 0., 1);
  lp = lp + exponential_lupdf(reciprocal_phi | 1);

  // ========== Repeat effect terms ==========
  lp = lp + normal_lupdf(gamma | hatGamma, 0.5);
  lp = lp + normal_lupdf(zeta | hatZeta, 0.1);
  lp = lp + normal_lupdf(eta | hatEta, 0.1);

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

