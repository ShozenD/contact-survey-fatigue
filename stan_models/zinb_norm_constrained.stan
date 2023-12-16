data {
  int<lower=1> N00; // No. of first-time participants with zero contacts
  int<lower=1> N10; // No. of repeating participants with zero contacts
  int<lower=1> N01; // No. of first-time participants with 1 or more contacts
  int<lower=1> N11; // No. of repeating participants with 1 or more contacts
  int<lower=1> P;  // Number of parameters
  matrix[N00,P] X00; // Design matrix
  matrix[N10,P] X10; // Design matrix
  matrix[N01,P] X01; // Design matrix
  matrix[N11,P] X11; // Design matrix
  array[N00] int y00; // Array of outcomes (first-time 0 contacts)
  array[N10] int y10; // Array of outcomes (repeating 0 contacts)
  array[N01] int y01; // Array of outcomes (first-time 1 or more contacts)
  array[N11] int y11; // Array of outcomes (repeating 1 or more contacts)
}

transformed data {
  int N = N00 + N10 + N01 + N11;
}

parameters {
  real alpha; // Global baseline
  vector[P] beta0; // Baseline coefficients
  vector<upper=0>[P] beta1; // Repeat coefficients
  real tau; // Constant connecting the log-link and the logit-link
  real<lower=0> nu; // Negative Binomial overdispersion parameter
}

model {
  // Priors
  target += normal_lpdf(alpha | 0, 5);
  target += normal_lpdf(beta0 | 0, 1);
  target += normal_lpdf(beta1 | 0, 1);
  target += normal_lpdf(tau | 0, 5);
  target += exponential_lpdf(nu | 1); // Overdispersion

  {
    vector[N00] log_lambda00 = X00*beta0;
    vector[N10] log_lambda10 = X10*beta0 + X10*beta1;
    vector[N01] log_lambda01 = X01*beta0;
    vector[N11] log_lambda11 = X11*beta0 + X11*beta1;

    vector[N00] theta00 = inv_logit(-tau*log_lambda00);
    vector[N10] theta10 = inv_logit(-tau*log_lambda10);
    vector[N01] theta01 = inv_logit(-tau*log_lambda01);
    vector[N11] theta11 = inv_logit(-tau*log_lambda11);

    // Update log-likelihood
    // Zero case
    for (i in 1:N00) {
      target += log_mix(theta00[i], 0, neg_binomial_lpmf(y00[i] | exp(log_lambda00[i])/nu, 1/nu));
      // target += log_sum_exp(log(theta00[i]), log1m(theta00[i]) + poisson_lpmf(y00[i] | exp(log_lambda00[i])));
    }
    for (i in 1:N10) {
      target += log_mix(theta10[i], 0, neg_binomial_lpmf(y10[i] | exp(log_lambda10[i])/nu, 1/nu));
      // target += log_sum_exp(log(theta10), log1m(theta10) + poisson_lpmf(y10 | exp(log_lambda10)));
    }

    // Non-zero case
    target += log1m(theta01);
    target += log1m(theta11);
    target += neg_binomial_lpmf(y01 | exp(log_lambda01)/nu, 1/nu);
    target += neg_binomial_lpmf(y11 | exp(log_lambda11)/nu, 1/nu);
  }
}

generated quantities {
  array[N] int yhat;

  {
    vector[N00] log_lambda00 = X00*beta0;
    vector[N10] log_lambda10 = X10*beta0 + X10*beta1;
    vector[N01] log_lambda01 = X01*beta0;
    vector[N11] log_lambda11 = X11*beta0 + X11*beta1;
    vector[N] log_lambda = append_row(log_lambda00, append_row(log_lambda10, append_row(log_lambda01, log_lambda11)));

    vector[N00] theta00 = inv_logit(-tau*log_lambda00);
    vector[N10] theta10 = inv_logit(-tau*log_lambda10);
    vector[N01] theta01 = inv_logit(-tau*log_lambda01);
    vector[N11] theta11 = inv_logit(-tau*log_lambda11);
    vector[N] theta = append_row(theta00, append_row(theta10, append_row(theta01, theta11)));

    for (i in 1:N) {
      real z = bernoulli_rng(1-theta[i]);
      if (z > 0) {
        yhat[i] = poisson_rng(exp(log_lambda[i]));
      } else {
        yhat[i] = 0;
      }
    }
  }
}
