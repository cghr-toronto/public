
data {
  int<lower=0> P;              // num beta
  int<lower=1> K;              // num city
  vector[P] betaKhat[K];               // beta data
  matrix[P,P] SigmaKhat[K];               // beta data
}

parameters {
  vector[P] kappa; // kappa in the model
  vector[P] betaK[K];
  cholesky_factor_corr[P] G; // G in the model
  vector<lower=0>[P] betaBar;
  
}
model {
  
  kappa[1] ~ exponential(200); // mean 0.005, or use exponential(2) for a mean of 0.5
  kappa[2] ~ exponential(200);
  kappa[3] ~ exponential(200);
 
  betaBar ~ normal(0,0.001);// this is the weak prior mentioned in the paper, for the comparison, can also use:betaBar ~ normal(0,100); 
  G ~ lkj_corr_cholesky(1);
  
     for(j in 1:K) {
     betaK[j] ~ multi_normal_cholesky(
       betaBar,
       diag_pre_multiply(kappa, G)
       );
    }
  

  for(i in 1:K)
  {
  betaKhat[i] ~ multi_normal(betaK[i], SigmaKhat[i]);
  }
}
