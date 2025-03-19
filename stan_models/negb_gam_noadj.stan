functions {
  // ===== Sum to zero priors =====
  matrix sum_zero_map(int n) {
    matrix[n, n-1] A;
    A[1:n-1, 1:n-1] = diag_matrix(ones_vector(n-1));
    A[n,:] = -1.0*ones_row_vector(n-1);

    // QR decomposition (Gram Schmidt)
    return qr_thin_Q(A);
  }
  
  vector sum_zero_std_normal(vector x) {
    int n = rows(x) + 1;
    vector[n] ones = ones_vector(n);
    matrix[n, n] In = identity_matrix(n);
    matrix[n, n] P = In - 1./(n*1.) * ones * ones';
    P = P * (n / (n-1.0));
    matrix[n, n-1] M = sum_zero_map(n);
    matrix[n-1, n-1] S = M' * P * M;
    matrix[n-1, n-1] L = cholesky_decompose(S);
    return M * (L * x);
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
  int<lower=1> P_sex;
  int<lower=1> P_hhsize;
  int<lower=1> P_job;
  int<lower=1> P_urbn;

  matrix[N, P_sex] X_sex;
  matrix[N, P_hhsize] X_hhsize;
  matrix[N, P_job] X_job;
  matrix[N, P_urbn] X_urbn;

  array[N] int<lower=1> aid;   // age index

  int<lower=1> M;
  real<lower=0> C;
  vector[A] x_hsgp;

  array[N] int<lower=0> y; // Array of contact reports
}

transformed data {
  real L = C * max(x_hsgp);
  matrix[A,M] phi = PHI(x_hsgp, L, M);
}

parameters {
  // Global baseline parameter
  real alpha;

  // Participant covariate parameters
  vector[P_sex-1] z_sex;  
  vector[P_hhsize-1] z_hhsize;
  vector[P_job-1] z_job;
  vector[P_urbn-1] z_urbn;

  // Reciprocal of the dispersion parameter
  real<lower=0> inv_varphi;

  // GP hyperparameters
  real<lower=0> lenscale;
  real<lower=0> sigma;
  vector[M] zb;
}

transformed parameters {
  vector[P_sex] beta_sex = sum_zero_std_normal(z_sex);
  vector[P_hhsize] beta_hhsize = sum_zero_std_normal(z_hhsize);
  vector[P_job] beta_job = sum_zero_std_normal(z_job);
  vector[P_urbn] beta_urbn = sum_zero_std_normal(z_urbn);

  vector[A] log_m = alpha + hsgp(zb, phi, sigma, lenscale, L);
  vector[N] log_lambda = log_m[aid] + X_sex*beta_sex + X_hhsize*beta_hhsize + X_job*beta_job + X_urbn*beta_urbn;
}

model {
  // Prior for the baseline parameter
  target += normal_lupdf(alpha | 0, 10)
  // Priors for the participant covariates
         + normal_lupdf(z_sex    | 0, 1)
         + normal_lupdf(z_hhsize | 0, 1)
         + normal_lupdf(z_job    | 0, 1)
         + normal_lupdf(z_urbn   | 0, 1)
  // Prior for the dispersion
         + exponential_lupdf(inv_varphi | 1)
  // GP hyperparameter priors
         + inv_gamma_lupdf(lenscale | 5, 1)
         + inv_gamma_lupdf(sigma    | 5, 1)
         + normal_lupdf(zb         | 0, 1)
  // Likelihood
         + neg_binomial_2_log_lupmf(y | log_lambda, 1 / inv_varphi);
}

generated quantities {
  real<lower=0> varphi = 1.0 / inv_varphi;
  array[N] int y_rep;
  vector[N] log_lik;

  for (i in 1:N) {
    y_rep[i] = neg_binomial_2_log_rng(log_lambda[i], varphi);
    log_lik[i] = neg_binomial_2_log_lpmf(y[i] | log_lambda[i], varphi);
  }
}

