functions {
  real repeat_effect(int j, int r, vector gamma, vector zeta, vector eta){
    real rho;
    if (j != 0) {
      rho = gamma[j] * (exp(zeta[j]) * r^eta[j]) / (1 + exp(zeta[j]) * r^eta[j]);
    } else {
      rho = 0;
    }

    return rho;
  }

  // ===== HSGP functions =====
  // Square root of the spectral density
  vector diagSPD_Matern52(real alpha, real rho, real L, int M) {
    return 2*alpha * sqrt(4.0/3) * (sqrt(5)/rho)^2.5 * inv((sqrt(5)/rho)^2 + ((pi()/2/L) * linspaced_vector(M, 1, M))^2)^1.5;
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

data
{
  // ========== Sample size and dimensions ==========
  int<lower=1> N;    // Number of participants
  int<lower=1> A;     // Number of age inputs
  int<lower=1> P;     // The number of participant covariates
  int<lower=1> J;     // The number of jobs with repeat effects
  int<lower=1> R;     // The maximum number of repeats

  // ========== Data ==========
  array[N] int Y;        // Array of contact reports
  matrix[N,P] X;         // Participant covariate design matrix
  array[N] int aid;      // Age index

  // ========== Repeat effect terms ==========
  array[N] int rid;  // Index for the number of repeats
  array[N] int jid;  // Job dummy variables (for repeat effects)
  real<lower=0> hatGamma;         // Shape parameter for gamma prior (posterior median of the longitudinal model)
  real hatZeta;          // Shape parameter for zeta prior (posterior median of the longitudinal model)
  real<lower=0> hatEta;           // Shape parameter for eta prior   (posterior median of the longitudinal model)

  // ========== HSGP ==========
  int<lower=1> M;  // Number of basis functions (participant age dimension)
  real<lower=0> C; // Factor to determine the boundary value L (participant age dimension)

  vector[A] x_hsgp; // Design matrix for HSGP
}

transformed data {
  // ========== HSGP ==========
  real L = C * max(x_hsgp);
  matrix[A,M] phi = PHI(x_hsgp, L, M); // HSGP Basis functions
}

parameters {
  real alpha;     // Baseline parameter
  real<lower=0> sigma_beta; // Participant covariate hierarchical variance parameter
  vector[P] beta;  // Participant covariate parameters
  real<lower=0> reciprocal_phi; // Reciprocal of the dispersion parameter

  // ========== Repeat effect terms ==========
  vector<lower=0>[J] gamma;
  vector[J] zeta;
  vector<lower=0>[J] eta;

  // ========== HSGP ==========
  real<lower=0> lenscale; // GP lengthscale
  real<lower=0> sigma;    // GP magnitude
  vector[M] zb;
}

transformed parameters {
  vector[N] log_lambda;
  vector[A] log_m; // Log contact intensity

  { // Local scope
    log_m = alpha + hsgp(zb, phi, sigma, lenscale, L);
    log_lambda = log_m[aid] + X*beta;

    // Add reporting fatigue adjustment factor
    for (i in 1:N) {
      log_lambda[i] = log_lambda[i] - repeat_effect(jid[i], rid[i], gamma, zeta, eta);
    }
  }
}

model {
  real lp = 0;

  // ========== Priors ==========
  lp = lp + normal_lupdf(alpha | 0., 10);
  lp = lp + cauchy_lupdf(sigma_beta | 0., 1);
  lp = lp + normal_lupdf(beta | 0., sigma_beta);
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
  target += lp + neg_binomial_2_log_lupmf(Y | log_lambda, 1.0/reciprocal_phi);
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;
  // Non-zero case
  for (i in 1:N) {
    log_lik[i] = neg_binomial_2_log_lpmf(Y[i] | log_lambda[i], 1.0/reciprocal_phi);
    yhat[i] = neg_binomial_2_log_rng(log_lambda[i], 1.0/reciprocal_phi);
  }
}

