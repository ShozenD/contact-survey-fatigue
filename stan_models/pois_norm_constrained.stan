data {
  int<lower=1> N0; // No. of first-time participants
  int<lower=1> N1; // No. of repeating participants
  int<lower=1> P;  // Number of parameters
  matrix[N0,P] X0; // Design matrix
  matrix[N1,P] X1; // Design matrix
  array[N0] int y0; // Array of outcomes (first-time)
  array[N1] int y1; // Array of outcomes (repeating)
}

transformed data {
  int N = N0 + N1;
  array[N] int y = append_array(y0, y1);
}

parameters {
  real alpha;
  vector[P] beta0; // Baseline coefficients
  vector<lower=0>[P] beta1; // Repeat coefficients
}

transformed parameters {

}

model {
  // Priors
  target += normal_lpdf(alpha | 0, 10);
  target += normal_lpdf(beta0 | 0, 1);
  target += normal_lpdf(beta1 | 0, 1);

  // Add a sum-to-zero soft constraint (for identifiabiliy)
  target += normal_lpdf(sum(beta0) | 0, 0.001*P);

  {
    vector[N0] log_lambda0 = X0*beta0;
    vector[N1] log_lambda1 = X1*beta0 - X1*beta1;
    vector[N] log_lambda = alpha + append_row(log_lambda0, log_lambda1);
    target += poisson_log_lpmf(y | log_lambda); // Update log-likelihood
  }
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;

  {
    vector[N0] log_lambda0 = X0*beta0;
    vector[N1] log_lambda1 = X1*beta0 - X1*beta1;
    vector[N] log_lambda = alpha + append_row(log_lambda0, log_lambda1);

    for (i in 1:N) {
      log_lik[i] = poisson_log_lpmf(y | log_lambda[i]);
    }

    yhat = poisson_log_rng(log_lambda);
  }
}
