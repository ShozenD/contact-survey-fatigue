functions
{
#include utils.stan
#include hsgp_functions_2D.stan
}

data
{
  // ========== Sample size and dimensions ==========
  int<lower=1> N;     // Number of observations

  int<lower=1> A;     // Number of age inputs
  int<lower=1> A2;    // A(A+1)/2
  int<lower=1> C;     // Number of age strata

  // ========== Data ==========
  array[N] int Y;        // Array of contact reports

  // ========== Age ==========
  matrix[A,C] age_strata_map;    // Matrix to map age to strata
  array[A*A] int sym_from_lowertri_idxset; //

  // ========== Offsets ==========
  vector[N] offN;       // Offset for sample size
  row_vector[A] offP;   // Offset for population size

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
  vector[N] log_offN = log(offN);
  matrix[A,A] log_offP = rep_matrix(log(offP), A);

  // ========== HSGP ==========
  real L1 = C1 * max(Xhsgp[:,1]);
  real L2 = C2 * max(Xhsgp[:,2]);

  // Precompute HSGP basis functions
  matrix[A2,M1*M2] PHI = PHI_2D(L1, L2, S, Xhsgp);         // Eigenfunctions
  matrix[M1*M2,2] sqrt_LAMBDA = sqrt_LAMBDA_2D(L1, L2, S); // Eigenvalues
}

parameters
{
  real alpha;     // Baseline parameter
  real<lower=0> reciprocal_phi; // Reciprocal of the dispersion parameter

  // ========== HSGP ==========
  real<lower=0> lenscale1; // GP lengthscale
  real<lower=0> lenscale2; // GP lengthscale
  real<lower=0> sigma;     // GP magnitude
  vector[M1*M2] z;
}

transformed parameters {
  vector[N] log_lambda;
  matrix[A,A] log_m; // logarithm of the contact intensity

  { // Local scope
    log_m = to_matrix(hsgp_m52_2d(z, sigma, lenscale1, lenscale2, sqrt_LAMBDA, PHI)[sym_from_lowertri_idxset], A, A) + log_offP;
    log_lambda = alpha + to_vector(log(exp(log_m)*age_strata_map)) + log_offN;
  }
}

model
{
  // ========== Priors ==========
  target += normal_lpdf(alpha | 0, 10);
  target += cauchy_lpdf(reciprocal_phi | 0., 1);

  // ========== HSGP ==========
  target += gamma_lpdf(lenscale1 | 5, 1);
  target += gamma_lpdf(lenscale2 | 5, 1);
  target += gamma_lpdf(sigma | 5, 5);
  target += normal_lpdf(z | 0, 1);

  // ========== Likelihood ==========
  target += neg_binomial_2_lpmf(Y | exp(log_lambda + 1e-4), 1.0/reciprocal_phi);
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;
  // Non-zero case
  for (i in 1:N) {
    log_lik[i] = neg_binomial_2_lpmf(Y[i] | exp(log_lambda[i] + 1e-4), 1.0/reciprocal_phi);
    yhat[i] = neg_binomial_2_rng(exp(log_lambda[i] + 1e-4), 1.0/reciprocal_phi);
  }
}

