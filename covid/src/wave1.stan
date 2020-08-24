data {
  int<lower=0> N;                // total number of observations (for all reporting units)
  int<lower=0> R;                // number of reporting units

  real time_index[N];            // time, in days
  int region_index[N];         

  int y_obs[N];                  // observated mortality counts 
  real logExpected[N];
}

parameters {
  // Skew-normal parameters
  real A[R];                     // the time/location peak 1
  real<lower=0> B[R];            // the duration of peak 1
  real<lower=0> C[R];            // the size of the peak1
  real<lower=0> K[R];            // skew parameter in the skew-normal (per reporting unit)
  // Other parameters
  real<lower=0> eta[R];           // spark term (for countries with low count periods)
  real<lower=0> inv_sqrt_phi[R];     // second parameter for negative binomial distribution (prior scale)
}

transformed parameters {
  real <lower=0> lambda[N];
  real <lower=0> phiLong[N];

  {
  real logKernel[N];  


    for(ii in 1:N) {

      phiLong[ii] = square(inv(inv_sqrt_phi[region_index[ii]]));
      
      logKernel[ii] = 
          log(C[region_index[ii]])  + logExpected[ii] +
           skew_normal_lpdf(time_index[ii]| A[region_index[ii]],
            B[region_index[ii]], K[region_index[ii]]);
            


      lambda[ii] = exp(logKernel[ii]) +  
        exp(logExpected[ii] + log(eta[region_index[ii]] ) );
    }
  }
}

model {
  // Priors 
  A ~ normal(18350,100);          // as.numeric(as.Date('2020/3/15'))
  B ~ gamma(10, 0.5);               // 4*B = 95% of cases in epidemic
  C ~ gamma(1.5, 0.36);            // total number of epidemic cases (scaled by Italy?)
  K ~ gamma(3, 0.5);             // skewness parameter


  inv_sqrt_phi ~ exponential(10);  // NB over-dispersion 
  eta ~ exponential(20000);         // spark term, mean is half a case per day in italy

  
  // Likelihood
  for (ii in 1:N) { 
    y_obs[ii] ~ neg_binomial_2(lambda[ii], phiLong[ii]); 
  }
}
