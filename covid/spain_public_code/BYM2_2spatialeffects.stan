functions {
  real icar_normal_lpdf(vector phi, int N, int[] node1, int[] node2, vector weight) {
  return -0.5 *  dot_product((weight .* (phi[node1] - phi[node2])), (phi[node1] - phi[node2]))
  + normal_lpdf(sum(phi) | 0, 0.001 * N);
  }
}

data {
  int<lower=0> N;
  int<lower=0> N_edges_movement;
  int<lower=0> N_edges_physical;
  int<lower=1, upper=N> node1_movement[N_edges_movement]; 
  int<lower=1, upper=N> node2_movement[N_edges_movement];
  int<lower=1, upper=N> node1_physical[N_edges_physical]; 
  int<lower=1, upper=N> node2_physical[N_edges_physical];
  int<lower=0> y[N]; // count outcomes
  vector<lower=0>[N] E; // exposure
  vector[N_edges_movement] weight_movement;
  vector[N_edges_physical] weight_physical;
  real s [2];
  
  int<lower=1> K; // num covariates
  matrix[N, K] x; // design matrix
 }
 
transformed data {
  vector[N] log_E = log(E);
}
 
 parameters {
  real beta0;
  vector[N] phi_movement;
  vector[N] phi_physical;
  vector[N] theta;
  vector[K] betas; // covariates
  simplex[3] rho; // proportion spatial variance
  real<lower=0> sigma; // overall standard deviation

}

transformed parameters {
 // real<lower=0, upper=1> rho = inv_logit(logit_rho);
  vector[N] convolved_re = sqrt(rho[1]/s[1]) * phi_movement + sqrt(rho[2]/s[2]) * phi_physical + sqrt(rho[3]) * theta;
}

 model {
  y ~ poisson_log(log_E + beta0 + x * betas + convolved_re*sigma);

  phi_movement ~ icar_normal_lpdf(N, node1_movement, node2_movement, weight_movement);
  phi_physical ~ icar_normal_lpdf(N, node1_physical, node2_physical, weight_physical);
  
  
  beta0 ~normal(0,1);
  betas ~ normal(0, 1);
  theta ~ normal(0, 1);
  sigma ~ normal(0, 1);
}

generated quantities{
  vector[N] eta = log_E +beta0 + x*betas + convolved_re * sigma;
//  int y_rep[N];

//  if (max(eta) > 20){
//    print("max eta too big: ", max(eta));
//    for (i in 1:N)
//    y_rep[i] = -1;
//   }else{
//     for (i in 1:N)
//    y_rep[i] = poisson_log_rng(eta[i]);
//  }
}

