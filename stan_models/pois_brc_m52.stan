functions
{
#include utils.stan
#include hsgp_functions_2D.stan

  real repeat_effect(int j, int r, vector gamma, vector kappa, vector eta){
    real rho;
    if (j != 0) {
      rho = gamma[j] * ( (1 - exp(-kappa[j] * r^eta[j])) / (1 + exp(-kappa[j] * r^eta[j])) );
    } else {
      rho = 0;
    }

    return rho;
  }
}

data
{
  // ========== Sample size and dimensions ==========
  int<lower=1> Nobs;     // Number of observations
  int<lower=1> Nzero;    // Number of zero observations
  int<lower=1> Nnonzero; // Number of nonzero observations
  int<lower=1> Npart;    // Number of participants

  int<lower=1> A;     // Number of age inputs
  int<lower=1> A2;    // A(A+1)/2
  int<lower=1> C;     // Number of age strata

  int<lower=1> P;     // The number of participant covariates
  int<lower=1> J;     // The number of jobs with repeat effects
  int<lower=1> R;     // The maximum number of repeats

  // ========== Data ==========
  array[Nobs] int Y;        // Array of contact reports
  matrix[Npart,P] X;        // Participant covariate design matrix
  array[Nobs] int part_idx; // Participant index

  // ========== Repeat effect terms ==========
  array[Npart] int rep_idx; // Index for the number of repeats
  array[Npart] int job_idx;     // Job dummy variables (for repeat effects)
  real hatGamma;            // Shape parameter for gamma prior (posterior median of the longitudinal model)
  real hatKappa;            // Shape parameter for kappa prior (posterior median of the longitudinal model)
  real hatEta;              // Shape parameter for eta prior   (posterior median of the longitudinal model)

  // ========== Age ==========
  array[Nobs,2] int age_idxset; // Matrix to map age effect to observations
  matrix[A,C] age_strata_map;    // Matrix to map age to strata
  array[A*A] int sym_from_lowertri_idxset; //

  // ========== Offsets ==========
  vector[A] offN;       // Offset for number of observations for age a
  row_vector[A] offP;   // Offset for population size
  vector[Npart] offS;   // Offset for missing and group contacts

  // ========== HSGP ==========
  int<lower=1> M1;  // Number of basis functions (participant age dimension)
  int<lower=1> M2;  // Number of basis functions (contact age dimension)
  real<lower=0> C1; // Factor to determine the boundary value L (participant age dimension)
  real<lower=0> C2; // Factor to determine the boundary value L for age of contacted individuals (contact age dimension)

  matrix[A2,2] Xhsgp; // Design matrix for HSGP
  matrix[M1*M2,2] S;   // Basis function combinations
}

transformed data
{
  real eps = 1e-13; // Prevent shape parameter to be 0

  // Precompute log offset terms
  vector[Npart] log_offS = log(offS);
  matrix[A,A] log_offP = rep_matrix(log(offP), A);
  matrix[A,C] log_offN = rep_matrix(log(offN), C);

  // ========== HSGP ==========
  real L1 = C1 * max(Xhsgp[:,1]);
  real L2 = C2 * max(Xhsgp[:,2]);

  // Precompute HSGP basis functions
  matrix[A2,M1*M2] PHI = PHI_2D(L1, L2, S, Xhsgp);              // Basis functions
  matrix[M1*M2,2] sqrt_LAMBDA = sqrt_LAMBDA_2D(L1, L2, S); // Eigenvalues
}

parameters
{
  real alpha;     // Baseline parameter
  vector[P] beta; // Participant covariate parameters

  // ========== Repeat effect terms ==========
  vector<lower=0>[J] gamma;
  vector<lower=0>[J] kappa;
  vector<lower=0>[J] eta;

  // ========== Individual Random effects ==========
  // real zeta_mean_hyper;
  // real<lower=0> zeta_sd_hyper;
  // vector[Npart] zeta;

  // ========== HSGP ==========
  real<lower=0> lenscale1; // GP lengthscale
  real<lower=0> lenscale2; // GP lengthscale
  real<lower=0> sigma;     // GP magnitude
  vector[M1*M2] z;
}

transformed parameters {
  vector[Nobs] log_lambda_obs;
  vector[Nobs] theta;
  matrix[A,A] log_m; // logarithm of the contact intensity

  { // Local scope
    log_m = to_matrix(hsgp_m52_2d(z, sigma, lenscale1, lenscale2, sqrt_LAMBDA, PHI)[sym_from_lowertri_idxset], A, A) + log_offP;
    matrix[A,C] log_lambda_strata = log(exp(log_m)*age_strata_map);

    vector[Npart] log_lambda_part;
    // log_lambda_part = alpha + X*beta + zeta + log_offS;
    log_lambda_part = alpha + X*beta + log_offS;
    for (i in 1:Npart) { log_lambda_part[i] = log_lambda_part[i] - repeat_effect(job_idx[i], rep_idx[i], gamma, kappa, eta); }
    log_lambda_obs = log_lambda_part[part_idx];

    for (i in 1:Nobs) { log_lambda_obs[i] = log_lambda_strata[age_idxset[i,1]][age_idxset[i,2]] + log_lambda_obs[i]; }
  }
}

model
{
  // ========== Priors ==========
  target += normal_lpdf(alpha | 0, 10);
  target += normal_lpdf(beta | 0, 1);

  // ========== Repeat effect terms ==========
  target += gamma_lpdf(gamma | hatGamma, 1);
  target += gamma_lpdf(kappa | hatKappa, 1);
  target += gamma_lpdf(eta | hatEta, 1);

  // ========== Individual Random effects ==========
  // target += normal_lpdf(zeta_mean_hyper | 0, 1);
  // target += normal_lpdf(zeta_sd_hyper | 0, 1);
  // target += normal_lpdf(zeta | zeta_mean_hyper, zeta_sd_hyper);

  // ========== HSGP ==========
  target += gamma_lpdf(lenscale1 | 5, 1);
  target += gamma_lpdf(lenscale2 | 5, 1);
  target += gamma_lpdf(sigma | 5, 5);
  target += normal_lpdf(z | 0, 1);

  // ========== Likelihood ==========
  target += poisson_log_lpmf(Y | log_lambda_obs + 1e-4);
}

generated quantities {
  array[Nobs] int yhat;
  vector[Nobs] log_lik;
  // Non-zero case
  for (i in 1:Nobs) {
    log_lik[i] = poisson_log_lpmf(Y[i] | log_lambda_obs[i] + 1e-4);
    yhat[i] = poisson_rng(exp(log_lambda_obs[i] + 1e-4));
  }
}

