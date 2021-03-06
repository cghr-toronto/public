data {
  int<lower=0> N;                // total number of observations (for all reporting units)
  int<lower=0> R;                // number of reporting units
  int<lower=0> M;                // number of countries
  int<lower=0> n_brazil;
  
  real time_index[N];            // number of days from November 1, 2019 for each reporting unit
  int region_index[N];           //
  int country_index[N];           //
  int brazil_index[n_brazil];
  int not_brazil_index[R-n_brazil]; //gross, how do I subset??

  int y_obs[N];                  // observated mortality counts
  real logExpected[N];
}

parameters {
  // Skew-normal parameters
  real A[R] ;
  real<lower=1> B[R];     
  real<lower=0> C[R];
  real<lower=0> K[R];
  real<lower=0> alpha[M];
  real<lower=0> eta[M];
  real<lower=0> zeta[M];
  real<lower=0> theta_c;
  real<lower=0> theta_b;
  real<lower=0> theta_k;

  // Other parameters
  real<lower=0> D[R];           // sparse term (for countries with low count periods)
  real<lower=0> inv_sqrt_phi[M];     // second parameter for negative binomial distribution (prior scale)
}

transformed parameters {
  real<lower=0> lambda[N];        // mean of the negative bionomial distribution (likelihood)
  real<lower=0> phi[M] = square(inv(inv_sqrt_phi)); // second parameter for NB distribution (natural scale)
  
  real logKernel[N];
  

    for(ii in 1:N) {

      logKernel[ii] = skew_normal_lpdf(time_index[ii]| A[region_index[ii]], B[region_index[ii]], K[region_index[ii]]);

      lambda[ii] = exp(logExpected[ii])*(C[region_index[ii]]*exp(logKernel[ii]) + D[region_index[ii]]);
    }
  
}

model {
  // Priors
  
  A[brazil_index] ~ normal(18430, 30);
  A[not_brazil_index]~ normal(18350, 100); 

  inv_sqrt_phi ~ exponential(10);
  D ~ exponential(10000); //fix this
  
  alpha ~ normal(50,40);
  eta ~ normal(60,30);
  zeta ~ normal(3,1);
  
  theta_b ~ normal(9, 3);
  theta_c ~ normal(2, 0.66);
  theta_k ~ normal(3, 1 );

for (r in 1:R){
  B[r] ~ gamma((alpha[country_index[r]]/theta_c), theta_c);
  C[r] ~ gamma(eta[country_index[r]]/theta_b, theta_b);
  K[r] ~ gamma(zeta[country_index[r]]/theta_k, theta_k);
}

  // Likelihood
  for (ii in 1:N) { y_obs[ii] ~ neg_binomial_2(lambda[ii], phi[country_index[ii]]); }
}
