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

model {
  // Priors
  target += normal_lpdf(alpha | 0, 5);
  target += normal_lpdf(beta0 | 0, 1);
  target += normal_lpdf(beta1 | 0, 1);

  {
    vector[N0] logit_pi0 = X0*beta0;
    vector[N1] logit_pi1 = X1*beta0 - X1*beta1;
    vector[N] logit_pi = alpha + append_row(logit_pi0, logit_pi1);
    target += bernoulli_logit_lpmf(y | logit_pi); // Update log-likelihood
  }
}

generated quantities {
  array[N] int yhat;
  vector[N] log_lik;

  {
    vector[N0] logit_pi0 = X0*beta0;
    vector[N1] logit_pi1 = X1*beta0 - X1*beta1;
    vector[N] logit_pi = alpha + append_row(logit_pi0, logit_pi1);

    for (i in 1:N) {
      log_lik[i] = bernoulli_logit_lpmf(y[i] | logit_pi[i]);
    }

    yhat = bernoulli_logit_rng(logit_pi);
  }
}
