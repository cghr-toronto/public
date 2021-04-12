functions {
  real icar_normal_lpdf(vector phi, int N, int[] node1, int[] node2, vector weight) {
  return -0.5 *  dot_product((weight .* (phi[node1] - phi[node2])), (phi[node1] - phi[node2]))
  + normal_lpdf(sum(phi) | 0, 0.001 * N);
  }
}

data {
  int<lower=0> N;
  int<lower=0> N_edges;
  int<lower=1, upper=N> node1[N_edges]; 
  int<lower=1, upper=N> node2[N_edges];
  int<lower=0> y[N]; // count outcomes
  vector<lower=0>[N] E; // exposure
  vector[N_edges] weight;
  real s;
  
  int<lower=1> K; // num covariates
  matrix[N, K] x; // design matrix
 }
 
transformed data {
  vector[N] log_E = log(E);
}
 
 parameters {
  real beta0;
  vector[N] phi;
  vector[N] theta;
  real logit_rho; // proportion spatial variance
  vector[K] betas; // covariates
  real<lower=0> sigma; // overall standard deviation
}

transformed parameters {
  real<lower=0, upper=1> rho = inv_logit(logit_rho);
  vector[N] convolved_re = sqrt(rho/s) * phi + sqrt(1 - rho) * theta;
}

 model {
  y ~ poisson_log(log_E + beta0 + x * betas + convolved_re*sigma);

  phi ~ icar_normal_lpdf(N, node1, node2, weight);
  
  beta0 ~normal(0,1);
  betas ~ normal(0, 1);
  theta ~ normal(0, 1);
  logit_rho ~ normal(0, 1);
  sigma ~ normal(0, 1);
}

generated quantities{
  vector[N] eta = log_E +beta0 + x*betas + convolved_re * sigma;
}