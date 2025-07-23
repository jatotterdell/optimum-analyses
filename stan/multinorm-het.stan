functions {
  int num_matches(array[] int x, int y) {
    int n = 0;
    for (i in 1 : num_elements(x)) 
      if (x[i] == y) 
        n += 1;
    return n;
  }
  
  array[] int which_equal(array[] int x, int y) {
    array[num_matches(x, y)] int match_positions;
    int pos = 1;
    for (i in 1 : num_elements(x)) {
      if (x[i] == y) {
        match_positions[pos] = i;
        pos += 1;
      }
    }
    return match_positions;
  }
  
  matrix sub_matrix(matrix M, array[] int idx) {
    int N = num_elements(idx);
    matrix[N, N] out;
    for (i in 1 : N) {
      for (j in 1 : N) {
        out[i, j] = M[idx[i], idx[j]];
      }
    }
    return out;
  }
}
data {
  int<lower=1> N; // subjects
  int<lower=1> J; // max observations per subject
  int P; // number of covariates
  matrix[N, P] X; // design matrix
  matrix[N, J] Y; // outcome
  array[N, J] int R; // (was outcome: 0 - observed, 1 - missing)
  array[N] int x; // treatment group
}
parameters {
  array[2] cholesky_factor_corr[J] L_Omega;
  array[2] vector<lower=0>[J] sigma;
  matrix[J, P] beta;
}
transformed parameters {
  array[2] cholesky_factor_cov[J] L_Sigma;
  array[2] corr_matrix[J] Omega;
  array[2] cov_matrix[J] Sigma;
  for (j in 1 : 2) {
    L_Sigma[j] = diag_pre_multiply(sigma[j], L_Omega[j]);
    Omega[j] = multiply_lower_tri_self_transpose(L_Omega[j]);
    Sigma[j] = quad_form_diag(Omega[j], sigma[j]);
  }
}
model {
  matrix[N, J] eta;
  
  //Priors
  for (j in 1 : 2) {
    target += cauchy_lpdf(sigma[j] | 0.0, 10.0) - cauchy_lccdf(0 | 0.0, 10.0);
    target += lkj_corr_cholesky_lpdf(L_Omega[j] | 1.0);
  }
  
  target += normal_lpdf(to_vector(beta) | 0.0, 10.0);
  
  // Likelihood
  for (n in 1 : N) {
    for (j in 1 : J) {
      eta[n, j] = X[n] * beta[j]';
    }
    int num_obs = num_matches(R[n], 0);
    if (num_obs == J) {
      target += multi_normal_lpdf(Y[n] | eta[n], Sigma[x[n]]);
    } else if (num_obs > 0) {
      array[num_obs] int idx_obs = which_equal(R[n], 0);
      matrix[num_obs, num_obs] S = sub_matrix(Sigma[x[n]], idx_obs);
      target += multi_normal_lpdf(Y[n, idx_obs] | eta[n, idx_obs], S);
    }
  }
}
generated quantities {
  matrix[N, J] eta;
  vector[N] log_lik;
  matrix[N, J] Ypred;
  
  for (n in 1 : N) {
    for (j in 1 : J) {
      eta[n, j] = X[n] * beta[j]';
    }
    int num_obs = num_matches(R[n], 0);
    if (num_obs == J) {
      log_lik[n] = multi_normal_lpdf(Y[n] | eta[n], Sigma[x[n]]);
    } else if (num_obs > 0) {
      array[num_obs] int idx_obs = which_equal(R[n], 0);
      matrix[num_obs, num_obs] S = sub_matrix(Sigma[x[n]], idx_obs);
      log_lik[n] = multi_normal_lpdf(Y[n, idx_obs] | eta[n, idx_obs], S);
    }
    Ypred[n] = to_row_vector(multi_normal_rng(eta[n], Sigma[x[n]]));
  }
}
