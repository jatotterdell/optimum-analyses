functions {

  int num_matches(array[,] int x, int y) {
    int n = 0;
    for (i in 1:dims(x)[1]) {
      for (j in 1:dims(x)[2]) {
        if (x[i,j] == y)
          n += 1;
      }
    }
    return n;
  }

  array[] int which_column(array[,] int x, int y) {
    array[num_matches(x, y)] int match_column;
    int pos = 1;
    for (i in 1:dims(x)[1]) {
      for (j in 1:dims(x)[2]) {
        if (x[i, j] == y) {
          match_column[pos] = j;
          pos += 1;
        }
      }
    }
    return match_column;
  }

}

data {
  int<lower=1> N; // subjects
  int<lower=2> J; // max observations per subject
  int P;          // number of covariates
  matrix[N, P] X; // design matrix
  matrix[N, J] Y; // outcome
  array[N, J] int R; // flag (was outcome: 0 - observed, 1 - missing, 2 - left-censored)
  vector[J] L; // upper limit
}

transformed data {
  int<lower=0> n_missing;
  int<lower=0> n_censored;

  n_missing = num_matches(R, 1);
  n_censored = num_matches(R, 2);

  array[n_censored] int cens_visit;
  cens_visit = which_column(R, 2);

  vector[n_censored] L_cens;
  L_cens = L[cens_visit];
}

parameters {
  cholesky_factor_corr[J] L_Omega;
  vector<lower=0>[J] sigma;
  matrix[J, P] b;
  vector[n_missing] y_missing;
  vector<upper=L_cens>[n_censored] y_censored;
}

transformed parameters{
  cholesky_factor_cov[J] L_Sigma;
  corr_matrix[J] Omega;
  cov_matrix[J] Sigma;
  L_Sigma = diag_pre_multiply(sigma, L_Omega);
  Omega = multiply_lower_tri_self_transpose(L_Omega);
  Sigma = quad_form_diag(Omega, sigma);
}

model {
  int pos_cens = 1;
  int pos_miss = 1;
  matrix[N, J] y;
  matrix[N, J] eta;

  //Priors
  sigma ~ cauchy(0.0, 10.0);
  L_Omega ~ lkj_corr_cholesky(1.0);
  to_vector(b) ~ normal(0.0, 10.0);

  // Likelihood
  for(n in 1:N) {
    for(j in 1:J) {
      eta[n, j] = X[n] * b[j]';
      if(R[n, j] == 0) {
        y[n, j] = Y[n, j];
      } else if (R[n, j] == 1) {
        y[n, j] = y_missing[pos_miss];
        pos_miss += 1;
      } else if (R[n, j] == 2) {
        y[n, j] = y_censored[pos_cens];
        pos_cens += 1;
      }
    }
    y[n] ~ multi_normal(eta[n], Sigma);
  }
}

generated quantities {
  matrix[N, J] eta;
  matrix[N, J] Ypred;

  for(n in 1:N) {
    for(j in 1:J) {
      eta[n, j] = X[n] * b[j]';
    }
    Ypred[n] = to_row_vector(multi_normal_rng(eta[n], Sigma));
  }
}
