functions
{
  real repeat_effect(int j, int r, vector gamma, vector kappa, vector eta){
    real rho;
    if (j != 0) {
      rho = gamma[j] * ( (1 - exp(-kappa[j] * r^eta[j])) / (1 + exp(-kappa[j] * r^eta[j])) );
    } else {
      rho = 0;
    }

    return rho;
  }

  // Square root of the spectral density
  vector diagSPD_Matern52(real alpha, real rho, real L, int M) {
    return 2*alpha * sqrt(4.0/3) * (sqrt(5)/rho)^2.5 * inv((sqrt(5)/rho)^2 + ((pi()/2/L) * linspaced_vector(M, 1, M))^2)^1.5;
  }

  // Basis functions
  matrix PHI(vector x, real L, int M) {
    return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L);
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
  real hatGamma;         // Shape parameter for gamma prior (posterior median of the longitudinal model)
  real hatKappa;         // Shape parameter for kappa prior (posterior median of the longitudinal model)
  real hatEta;           // Shape parameter for eta prior   (posterior median of the longitudinal model)

  // ========== HSGP ==========
  int<lower=1> M;  // Number of basis functions (participant age dimension)
  real<lower=0> C; // Factor to determine the boundary value L (participant age dimension)

  vector[A] x_hsgp; // Design matrix for HSGP
}

transformed data
{
  real eps = 1e-13; // Prevent shape parameter to be 0

  // ========== HSGP ==========
  real L = C * max(x_hsgp);

  // Precompute HSGP basis functions
  matrix[A,M] phi = PHI(x_hsgp, L, M); // Basis functions
}

parameters
{
  real alpha;     // Baseline parameter
  vector[P] beta; // Participant covariate parameters
  real<lower=0> reciprocal_phi; // Reciprocal of the dispersion parameter

  // ========== Repeat effect terms ==========
  vector<lower=0>[J] gamma;
  vector<lower=0>[J] kappa;
  vector<lower=0>[J] eta;

  // ========== HSGP ==========
  real<lower=0> lenscale; // GP lengthscale
  real<lower=0> sigma;    // GP magnitude
  vector[M] z;
}

transformed parameters {
  vector[N] log_lambda;
  vector[A] log_m; // Log contact intensity

  { // Local scope
    log_m = alpha + diag_post_multiply(phi, diagSPD_Matern52(sigma, lenscale, L, M))*z;
    log_lambda = log_m[aid] + X*beta;

    // Add reporting fatigue adjustment factor
    for (i in 1:N) {
      log_lambda[i] = log_lambda[i] - repeat_effect(jid[i], rid[i], gamma, kappa, eta);
    }
  }
}

model
{
  // ========== Priors ==========
  target += normal_lpdf(alpha | 0, 10);
  target += normal_lpdf(beta | 0, 1);
  target += cauchy_lpdf(reciprocal_phi | 0., 1);

  // ========== Repeat effect terms ==========
  target += gamma_lpdf(gamma | hatGamma, 1);
  target += gamma_lpdf(kappa | hatKappa, 1);
  target += gamma_lpdf(eta | hatEta, 1);

  // ========== HSGP ==========
  target += inv_gamma_lpdf(lenscale | 5, 1);
  target += inv_gamma_lpdf(sigma | 5, 1);
  target += normal_lpdf(z | 0, 1);

  // ========== Likelihood ==========
  target += neg_binomial_2_log_lpmf(Y | log_lambda + eps, 1.0/reciprocal_phi);
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;
  // Non-zero case
  for (i in 1:N) {
    log_lik[i] = neg_binomial_2_log_lpmf(Y[i] | log_lambda[i] + eps, 1.0/reciprocal_phi);
    yhat[i] = neg_binomial_2_log_rng(log_lambda[i] + eps, 1.0/reciprocal_phi);
  }
}

