data {
  int<lower=0> N;                  // Number of observations
  int<lower=0> K;                  // Number of predictors
  int<lower=0> G;                  // Number of groups (random intercepts)
  int<lower=1, upper=G> z[N];  // Group indicator for each observation
  int<lower=1> D;                  // Dimensionality of the response vector
  matrix[N, K] X;                  // Predictor matrix
  matrix[N, D] Y;                  // Response matrix (each row is a D-dimensional response)
}

parameters {
  matrix[K, D] beta;               // Fixed effects coefficients (K predictors by D response dimensions)
  vector<lower=0>[D] sigma_y;      // Residual standard deviations for each dimension
  matrix[G, D] alpha;              // Random intercepts for each group (G groups by D response dimensions)
  vector<lower=0>[D] sigma_alpha;  // Standard deviations of random intercepts for each dimension
  cholesky_factor_corr[D] L_Omega; // Cholesky factor of the correlation matrix for the multivariate normal residuals
}

model {
  // Priors
  to_vector(beta) ~ normal(0, 5); 
  sigma_y ~ normal(0, 5);
  to_vector(alpha) ~ normal(0, 1); // Standardized intercepts
  sigma_alpha ~ normal(0, 5);
  L_Omega ~ lkj_corr_cholesky(1);

  // Likelihood
  for (n in 1:N) {
    vector[D] mu[N];
     mu = X[n] * beta + alpha[z[n]];
    Y[n] ~ multi_normal_cholesky(mu, diag_pre_multiply(sigma_y, L_Omega));
  }
}
