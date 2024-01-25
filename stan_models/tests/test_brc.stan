functions {
#include ../utils.stan
#include ../hsgp_functions_2D.stan

  real repeat_effect(int j, int r, vector gamma, vector kappa, vector eta){
    int J = num_elements(gamma);
    real rho;

    if (j != 0) {
      rho = gamma[j] * ( (1 - exp(-kappa[j] * r^eta[j])) / (1 + exp(-kappa[j] * r^eta[j])) );
    } else {
      rho = 0;
    }

    return rho;
  }
}

data {
  // ========== Sample size and dimensions ==========
  int<lower=1> Nobs;  // Number of observations
  int<lower=1> Npart; // Number of participants

  int<lower=1> A;     // Number of age inputs
  int<lower=1> A2;    // A*(A+1)/2
  int<lower=1> C;     // Number of age strata

  int<lower=1> J;     // The number of jobs with repeat effects
  int<lower=1> R;     // The maximum number of repeats

  array[Nobs] int part_idx; // Participant index
  array[Nobs,2] int age_idxset; // Matrix to map age effect to observations

   // ========== Repeat effect terms ==========
  array[Npart] int rep_idx; // Index for the number of repeats
  array[Npart] int job_idx;     // Job dummy variables (for repeat effects)
  real hatGamma;            // Shape parameter for gamma prior (posterior median of the longitudinal model)
  real hatKappa;            // Shape parameter for kappa prior (posterior median of the longitudinal model)
  real hatEta;              // Shape parameter for eta prior   (posterior median of the longitudinal model)

  // ========== HSGP ==========
  int<lower=1> M1;  // Number of basis functions (participant age dimension)
  int<lower=1> M2;  // Number of basis functions (contact age dimension)
  real<lower=0> C1; // Factor to determine the boundary value L (participant age dimension)
  real<lower=0> C2; // Factor to determine the boundary value L for age of contacted individuals (contact age dimension)

  matrix[A2,2] Xhsgp; // Design matrix for HSGP
  matrix[M1*M2,2] S;   // Basis function combinations
  array[A*A] int sym_from_lowertri_idxset;
  matrix[A,C] age_strata_map;    // Matrix to map age to strata

  // ========== Offsets ==========
  row_vector[A] offP; // Population offsets
}

transformed data {
  // ========== HSGP ==========
  real L1 = C1 * max(Xhsgp[:,1]);
  real L2 = C2 * max(Xhsgp[:,2]);

  // Precompute HSGP basis functions
  matrix[A2,M1*M2] PHI = PHI_2D(L1, L2, S, Xhsgp);              // Basis functions
  matrix[M1*M2,2] sqrt_LAMBDA = sqrt_LAMBDA_2D(L1, L2, S); // Eigenvalues

  matrix[A,A] log_offP = rep_matrix(log(offP), A); // Population offsets
}

transformed parameters {
  real sigma = 1;
  real lenscale1 = 1;
  real lenscale2 = 1;
}

generated quantities {
  vector[Nobs] lambda_obs;

  vector[M1*M2] z = multi_normal_rng(zeros_row_vector(M1*M2), diag_matrix(ones_vector(M1*M2)));

  vector<lower=0>[J] gamma;
  vector<lower=0>[J] kappa;
  vector<lower=0>[J] eta;
  for (j in 1:J) {
    gamma[j] = gamma_rng(hatGamma, 1);
    kappa[j] = gamma_rng(hatKappa, 1);
    eta[j] = gamma_rng(hatEta, 1);
  }

  {
    vector[Npart] lambda_part;
    for (i in 1:Npart){ lambda_part[i] = repeat_effect(job_idx[i], rep_idx[i], gamma, kappa, eta); }
    lambda_obs = lambda_part[part_idx];

    matrix[A,A] log_m = to_matrix(hsgp_se_2d(z, sigma, lenscale1, lenscale2, sqrt_LAMBDA, PHI)[sym_from_lowertri_idxset], A, A) + log_offP;
    matrix[A,C] m_strata = log_m * age_strata_map;

    for (i in 1:Nobs) { lambda_obs[i] = m_strata[age_idxset[i,1], age_idxset[i,2]] * lambda_obs[i]; }
  }
}

