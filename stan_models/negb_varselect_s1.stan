functions {
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

  vector sum_zero_horseshoe(vector x, vector lambda, real tau, real slab) {
    int n = rows(x) + 1;
    vector[n] lambda_tilde = sqrt(slab * lambda.^2 ./ (slab + tau^2 * lambda.^2));
    vector[n] lambda2_tilde = square(lambda_tilde);
    real lambda2_tilde_sum = sum(lambda2_tilde);

    // Scaling factor:
    real c = tau * sqrt( n * 1.0 / (n-1) );
    // Get orthonormal basis for sum-to-zero subspace
    matrix[n, n-1] M = sum_zero_map(n);

    // -----------------------------------------------------------
    // PART 1: Compute M' * [diag(lam_sqr)] * M
    // without forming the large diag_matrix(lambda2_tilde).
    //
    // diag_pre_multiply(lambda, M) multiplies row i of M by lambda[i].
    // The crossprod(...) = A' * A pattern then yields the sum_i lam[i]^2 * M[i,k] * M[i,l].
    // -----------------------------------------------------------
    matrix[n, n - 1] tmp = diag_pre_multiply(lambda_tilde, M);
    matrix[n - 1, n - 1] part1 = crossprod(tmp);

    // -----------------------------------------------------------
    // PART 2: The rank-1 update: M' * Lam*1 * (1'Lam*1)^-1 * 1'Lam * M
    //
    // But 'Lam * 1' is basically just 'lambda_tilde' (since 1 is all ones),
    // so we only need M' * lambda_tilde and lambda_tilde' * M.
    // Then we scale by 1 / sum(lam_sqr).
    // -----------------------------------------------------------
    vector[n - 1] w = M' * lambda2_tilde; //"should be lam_sqr"
    row_vector[n - 1] u = lambda2_tilde' * M;
    matrix[n - 1, n - 1] part2 = w * u / lambda2_tilde_sum;

    // Combine then scale by c^2
    matrix[n - 1, n - 1] cov = (part1 - part2) * (c^2);


    // Cholesky factor in (n-1) x (n-1)
    matrix[n-1, n-1] L = cholesky_decompose(cov);

    // Finally map back to R^n
    return M * (L * x);
  }
}

data {
  /* ===== Sample size ===== */
  int<lower=1> N;     // Total number of participants

  /* ===== Outcomes ===== */
  array[N] int y;   // Observed counts

  /* ===== Design matrices ===== */
  int<lower=1> P_age;
  int<lower=1> P_hh;
  int<lower=1> P_gender;

  matrix[N,P_age] U_age;
  matrix[N,P_hh] U_hh;
  matrix[N,P_gender] U_gender;

  int<lower=1> P_job;
  int<lower=1> P_symp;
  int<lower=1> P_dow;
  int<lower=1> P_urbn;

  matrix[N,P_job] V_job;
  matrix[N,P_symp] V_symp;
  matrix[N,P_dow] V_dow;
  matrix[N,P_urbn] V_urbn;
}

parameters {
  real beta0; // Global baseline parameter
  real<lower=0> inverse_phi;

  /* ===== Fixed effects ===== */
  // Horseshoe prior
  vector[P_age-1] z_age;
  vector[P_hh-1] z_hh;
  vector[P_gender-1] z_gender;

  vector[P_job-1] z_job;
  vector[P_symp-1] z_symp;
  vector[P_dow-1] z_dow;
  vector[P_urbn-1] z_urbn;

  vector<lower=0>[P_job] lambda_job;
  vector<lower=0>[P_symp] lambda_symp;
  vector<lower=0>[P_dow] lambda_dow;
  vector<lower=0>[P_urbn] lambda_urbn;

  real<lower=0> tau_job;
  real<lower=0> tau_symp;
  real<lower=0> tau_dow;
  real<lower=0> tau_urbn;

  real<lower=0> slab_job;
  real<lower=0> slab_symp;
  real<lower=0> slab_dow;
  real<lower=0> slab_urbn;
}

transformed parameters {
  /* ===== Fixed effects ===== */
  vector[P_age] alpha_age = sum_zero_std_normal(z_age);
  vector[P_hh] alpha_hh = sum_zero_std_normal(z_hh);
  vector[P_gender] alpha_gender = sum_zero_std_normal(z_gender);

  vector[P_job] beta_job = sum_zero_horseshoe(z_job, lambda_job, tau_job, slab_job);
  vector[P_symp] beta_symp = sum_zero_horseshoe(z_symp, lambda_symp, tau_symp, slab_symp);
  vector[P_dow] beta_dow = sum_zero_horseshoe(z_dow, lambda_dow, tau_dow, slab_dow);
  vector[P_urbn] beta_urbn = sum_zero_horseshoe(z_urbn, lambda_urbn, tau_urbn, slab_urbn);

  vector[N] log_lambda = beta0 + U_age*alpha_age + U_hh*alpha_hh + U_gender*alpha_gender;
  log_lambda = log_lambda + V_job*beta_job + V_symp*beta_symp + V_dow*beta_dow + V_urbn*beta_urbn;
}

model {
  // Priors
  target += normal_lpdf(beta0 | 0, 10);
  target += exponential_lpdf(inverse_phi | 1);

  target += normal_lpdf(z_age | 0, 1);
  target += normal_lpdf(z_hh | 0, 1);
  target += normal_lpdf(z_gender | 0, 1);

  /* ===== Horseshoe priors ===== */
  target += normal_lpdf(z_job | 0, 1);
  target += normal_lpdf(z_symp | 0, 1);
  target += normal_lpdf(z_dow | 0, 1);
  target += normal_lpdf(z_urbn | 0, 1);

  target += student_t_lpdf(lambda_job  | 3, 0, 1) - rows(lambda_job) * log(0.5);
  target += student_t_lpdf(lambda_symp  | 3, 0, 1) - rows(lambda_symp) * log(0.5);
  target += student_t_lpdf(lambda_dow  | 3, 0, 1) - rows(lambda_dow) * log(0.5);
  target += student_t_lpdf(lambda_urbn  | 3, 0, 1) - rows(lambda_urbn) * log(0.5);

  target += student_t_lpdf(tau_job | 4, 0, (P_job - 1.0)/sqrt(N));
  target += student_t_lpdf(tau_symp | 4, 0, (P_symp - 1.0)/sqrt(N));
  target += student_t_lpdf(tau_dow | 4, 0, (P_dow - 1.0)/sqrt(N));
  target += student_t_lpdf(tau_urbn | 4, 0, (P_urbn - 1.0)/sqrt(N));

  target += inv_gamma_lpdf(slab_job | 1, 2);
  target += inv_gamma_lpdf(slab_symp | 1, 2);
  target += inv_gamma_lpdf(slab_dow | 1, 2);
  target += inv_gamma_lpdf(slab_urbn | 1, 2);

  // ===== Likelihood =====
  target += neg_binomial_2_log_lpmf(y | log_lambda, 1/inverse_phi);
}

generated quantities {
  array[N] int yhat;
  array[N] real log_lik;

  for (i in 1:N) {
    yhat[i] = neg_binomial_2_log_rng(log_lambda[i], 1/inverse_phi);
    log_lik[i] = neg_binomial_2_log_lpmf(y | log_lambda[i], 1/inverse_phi);
  }
}
