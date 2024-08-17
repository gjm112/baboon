//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> K;
  int<lower=1> D;
  int<lower=0> N;
  int<lower=0> G;
  vector[K] X[N];
  vector[D] Y[N];
  int<lower=0> z[N];
}

parameters {
  matrix[D,K] beta;
  matrix[D,G] b;
  cov_matrix[D] Sigma;
}
model {
  vector[K] mu[N];
  for (n in 1:N)
    mu[n] = beta*X[n] + b[,z[n]];
  Y ~ multi_normal(mu, Sigma);
}


