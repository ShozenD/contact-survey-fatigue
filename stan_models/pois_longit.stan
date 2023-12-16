data {
  int<lower=0> N;        // number of observations
  int<lower=0> N_part;   // The number of unique participants
  int<lower=0> N_wave;   // The number of waves
  int<lower=0> N_repeat; // The number of repeats
  int<lower=0> P;        // number of predictors (dummies)
  matrix[N,P] X;         // dummy variables predictor matrix
  array[N] int part_idx; // participant index
  array[N] int w_idx;       // Wave variable
  array[N] int r_idx;       // Repeat variable (may be transformed in a non-linear fashion)
  vector[N] y_lag;       // lagged response variable (AR1)

  array[N] int y;           // response variable
}

parameters {
  real alpha;             // intercept
  vector[P] beta;         // dummy coefficients
  vector[N_wave] tau;     // wave coefficients
  vector[N_repeat-1] rho1;   // repeat coefficients
  vector[N_part] phi;     // Auto-regressive parameter for participant
}

transformed parameters {
  // Linear predictor
  vector[N_repeat] rho2;
  rho2[1] = 0;
  rho2[2:N_repeat] = rho1;

  vector[N] log_lambda; // log rate
  log_lambda = alpha + X * beta + tau[w_idx] + phi[part_idx] .* y_lag + rho2[r_idx];
}

model {
  // priors
  target += normal_lpdf(alpha | 0, 10);
  target += normal_lpdf(beta | 0, 1);
  target += normal_lpdf(rho1 | 0, 1);
  target += normal_lpdf(phi | 0, 1);

  target += normal_lpdf(tau[1] | 0, 1);
  target += normal_lpdf(tau[2:N_wave] | tau[1:N_wave-1], 1);

  // likelihood
  target += poisson_log_lpmf(y | log_lambda);
}

generated quantities {
  array[N] int yhat;   // posterior predictions
  array[N] real log_lik; // log likelihood

  for (n in 1:N) {
    yhat[n] = poisson_log_rng(log_lambda[n]);
    log_lik[n] = poisson_log_lpmf(y[n] | log_lambda[n]);
  }
}
