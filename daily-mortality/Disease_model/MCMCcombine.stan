
data {
  int<lower=0> P;              // num beta
  int<lower=1> K;              // num city
  matrix[K, P] betaKhat;               // beta data
  matrix[P,P] SigmaKhat[K];               // beta data
}

parameters {
  real<lower=0> kappa; // kappa in the model
  vector[P] betaK[K];
  cholesky_factor_corr[P] G; // G in the model
  vector [P] betaBar;
  
}
model {
  
  kappa ~ exponential(0.5);
  betaBar ~ normal(0,100);
  G ~ lkj_corr_cholesky(1);
  
     for(j in 1:K)
    {
     betaK[j] ~ multi_normal_cholesky(betaBar,diag_pre_multiply(rep_vector(kappa, P),G));
    }
  

  for(i in 1:K)
  {
  betaKhat[i] ~ multi_normal(betaK[i], SigmaKhat[i]);
  }
}
