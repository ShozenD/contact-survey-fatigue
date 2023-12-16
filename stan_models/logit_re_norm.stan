data {
  int<lower=1> N; // Sample size
  int<lower=1> P; // Number of parameters
  matrix[N,P] X; // Design matrix
  array[N] int y; // Array of outcomes
}

parameters {
  real alpha; // Baseline coefficient
  vector[P] beta; // Coefficients
}

model {
  // Priors
  target += normal_lpdf(alpha | 0, 5);
  target += normal_lpdf(beta | 0, 1);

  // Log likelihood
  target += poisson_log_glm_lpmf(y | X, alpha, beta);
}
