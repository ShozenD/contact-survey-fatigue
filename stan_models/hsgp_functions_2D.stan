vector lambda(real L, vector m) {
  return square(m * pi() / (2 * L));
}

matrix sqrt_LAMBDA_2D(real L1, real L2, matrix S) {
  matrix[rows(S),2] sqrt_LAMBDA;
  sqrt_LAMBDA[:,1] = sqrt(lambda(L1, S[,1]));
  sqrt_LAMBDA[:,2] = sqrt(lambda(L1, S[,2]));

  return sqrt_LAMBDA;
}

real spd_se_2d(real alpha, real rho1, real rho2, row_vector sqrt_lambda) {
  return alpha * 2 * pi() * rho1 * rho2 * square(exp(-0.5 * dot_product(square([rho1, rho2]'), to_vector(sqrt_lambda))));
}

vector diagSPD_EQ_2D(real alpha, real rho1, real rho2, matrix sqrt_LAMBDA) {
  int J = rows(sqrt_LAMBDA);
  vector[J] diagSPD;
  for (j in 1:J) {
    diagSPD[j] = sqrt(spd_se_2d(alpha, rho1, rho2, sqrt_LAMBDA[j]));
  }

  return diagSPD;
}

matrix PHI_2D(real L1, real L2, matrix S, matrix X) {
	int N = rows(X);
	int J = rows(S);

	// Precompute to avoid repetitive calculations in loop
	real inv_sqrt_L1 = 1/sqrt(L1);
	real inv_sqrt_L2 = 1/sqrt(L2);
	matrix[J,2] sqrt_LAMBDA = sqrt_LAMBDA_2D(L1, L2, S);
	vector[N] x1_plus_L1 = X[,1] + L1;
	vector[N] x2_plus_L2 = X[,2] + L2;

	matrix[N,J] PHI;
  for (j in 1:J) {
		vector[N] phi_j1 = inv_sqrt_L1 * sin(sqrt_LAMBDA[j,1] * x1_plus_L1);
		vector[N] phi_j2 = inv_sqrt_L2 * sin(sqrt_LAMBDA[j,2] * x2_plus_L2);
		PHI[,j] = phi_j1 .* phi_j2;
  }

  return PHI;
}

vector hsgp_se_2d(vector beta, real alpha, real rho1, real rho2, matrix sqrt_LAMBDA, matrix PHI) {
  vector[rows(sqrt_LAMBDA)] diagSPD = diagSPD_EQ_2D(alpha, rho1, rho2, sqrt_LAMBDA);

  return PHI * (diagSPD .* beta);
}

