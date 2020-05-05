functions {
vector lp_reduce(vector total_param, vector theta, real[] xr, int[] xi) {
  
  int temp_leng=size(xi);
  int M=xi[temp_leng-11];
  int K=xi[temp_leng-10];
  int N_p=xi[temp_leng-9];
  int N_s=xi[temp_leng-8];
  int Nh=xi[temp_leng-7];
  int Nw=xi[temp_leng-6];
  int Nd=xi[temp_leng-5];
  int N_rho=xi[temp_leng-4];
  int para_hour=xi[temp_leng-3];
  int para_cens=xi[temp_leng-2];
  int para_comp=xi[temp_leng-1];
  int trend_reference_day=xi[temp_leng];

  
  matrix[N_p, Nh] factor_h; //
  matrix[N_p, Nw] factor_w;
  matrix[Nd, N_p] factor_d;
  matrix[N_s, N_p] factor_q;
  matrix[Nd, N_p] factor_c[N_s];
  vector[N_p] miu;// why not using matrix? because the prior easy to write?
  vector[N_p] factor_trend;// time trend
  matrix[N_p, K] betas; //the regression parameters
  vector[N_p] alpha ; // alpha parameter for gamma distribution
  matrix[N_p, N_rho] mixture_rho;

  int pollutant[M*para_hour];
  int station[M*para_hour];
  int day[M*para_hour];
  int rho_index[M*para_hour];
  int hour[M*para_hour];
  int week[M*para_hour];
  
  int pollutant_cens[M*para_cens];
  int station_cens[M*para_cens];
  int day_cens[M*para_cens];
  int rho_index_cens[M*para_cens];
  int hour_cens[M*para_cens];
  int week_cens[M*para_cens];
  
  int pollutant_complete[M*para_comp];
  int station_complete[M*para_comp];
  int day_complete[M*para_comp];
  int H_complete[M*para_comp];
  int week_complete[M*para_comp];
  
  real y[M*para_hour];
  real sumlog[M*para_comp];
  real sumobs[M*para_comp];
  matrix[M*para_hour, K] x; // predictor matrix
  matrix[M*para_cens, K] x_cens; // predictor matrix
  matrix[M*para_comp, K] x_complete; // predictor matrix
  real censored_threshold;
  real cauchy_scale[N_p];
    
  real lp=0.0;
  
  
  station=xi[1:M*para_hour];
  pollutant=xi[(M*para_hour+1):(2*M*para_hour)];
  hour=xi[(2*M*para_hour+1):(3*M*para_hour)];
  week=xi[(3*M*para_hour+1):(4*M*para_hour)];
  day=xi[(4*M*para_hour+1):(5*M*para_hour)];
  rho_index=xi[(5*M*para_hour+1):(6*M*para_hour)];
  
  station_cens = xi[(6*M*para_hour+1):(6*M*para_hour+M*para_cens)];
  pollutant_cens =   xi[(6*M*para_hour+M*para_cens+1):(6*M*para_hour+2*M*para_cens)];
  hour_cens =  xi[(6*M*para_hour+2*M*para_cens+1):(6*M*para_hour+3*M*para_cens)];
  week_cens =  xi[(6*M*para_hour+3*M*para_cens+1):(6*M*para_hour+4*M*para_cens)];
  day_cens =  xi[(6*M*para_hour+4*M*para_cens+1):(6*M*para_hour+5*M*para_cens)];
  rho_index_cens =  xi[(6*M*para_hour+5*M*para_cens+1):(6*M*para_hour+6*M*para_cens)];
    
  station_complete = xi[(6*M*para_hour+6*M*para_cens+1):(6*M*para_hour+6*M*para_cens+M*para_comp)];
  pollutant_complete = xi[(6*M*para_hour+6*M*para_cens+M*para_comp+1):(6*M*para_hour+6*M*para_cens+2*M*para_comp)];
  H_complete =  xi[(6*M*para_hour+6*M*para_cens+2*M*para_comp+1):(6*M*para_hour+6*M*para_cens+3*M*para_comp)];
  week_complete = xi[(6*M*para_hour+6*M*para_cens+3*M*para_comp+1):(6*M*para_hour+6*M*para_cens+4*M*para_comp)];
  day_complete = xi[(6*M*para_hour+6*M*para_cens+4*M*para_comp+1):(6*M*para_hour+6*M*para_cens+5*M*para_comp)];
    

   y = xr[ 1:(M*para_hour)];
   sumlog =  xr[ (M*para_hour+1):(M*para_hour+M*para_comp)];
   sumobs = xr[ (M*para_hour+M*para_comp+1):(M*para_hour+2*M*para_comp)];
   x=to_matrix(xr[ (M*para_hour+2*M*para_comp+1):(M*para_hour+2*M*para_comp+M*para_hour*K)], M*para_hour, K);
   x_cens=to_matrix(xr[ (M*para_hour+2*M*para_comp+M*para_hour*K+1):(M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K)], M*para_cens, K);
   x_complete=to_matrix(xr[ (M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+1):(M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K)], M*para_comp, K);
   factor_h=to_matrix(xr[ (M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+1):(M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+N_p*Nh)],N_p, Nh);
   censored_threshold=xr[ (M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+N_p*Nh+1)];
   cauchy_scale=xr[ (M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+N_p*Nh+1+1):(M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+N_p*Nh+1+N_p)];
  
  mixture_rho= to_matrix(total_param[1:(N_rho*N_p)],N_p,N_rho);
  alpha= total_param[(N_rho*N_p+1):(N_rho*N_p+N_p)];
  miu= total_param[(N_rho*N_p+N_p+1):(N_rho*N_p+2*N_p)];
  
  factor_q= to_matrix(total_param[(N_rho*N_p+2*N_p+1):(N_rho*N_p+2*N_p+N_s*N_p)], N_s, N_p);
  betas=to_matrix(total_param[(N_rho*N_p+2*N_p+N_s*N_p+1):(N_rho*N_p+2*N_p+N_s*N_p+N_p*K)],N_p, K);
  
  factor_w = to_matrix(total_param[(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+1):(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+ N_p*Nw)], N_p, Nw);
  
  for(i in 1:N_s)
  {
  factor_c[i]=to_matrix(total_param[(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+1+(i-1)*Nd*N_p):(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+i*Nd*N_p)], Nd, N_p);
  }
  factor_d=to_matrix(total_param[((N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+N_s*Nd*N_p)+1):(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+N_s*Nd*N_p+Nd*N_p)],Nd, N_p);
  
  factor_trend=total_param[(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+N_s*Nd*N_p+Nd*N_p+1):(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+N_s*Nd*N_p+Nd*N_p+N_p)];


 for (n in 1:M){
   // ########################
   for(para in 1:para_hour)
   {
     
                lp += log_mix(mixture_rho[pollutant[n+(para-1)*M],rho_index[n+(para-1)*M]],
                                 gamma_lpdf(y[n+(para-1)*M] | alpha[pollutant[n+(para-1)*M]], (alpha[pollutant[n+(para-1)*M]] / exp(miu[pollutant[n+(para-1)*M]]+ factor_q[station[n+(para-1)*M],pollutant[n+(para-1)*M]] + x[n,] * betas[pollutant[n+(para-1)*M]]' + 
                                 + factor_trend[pollutant[n+(para-1)*M]]*((day[n+(para-1)*M]-trend_reference_day)/3652.5)+
                                         // factor_h[pollutant[n+(para-1)*M],hour[n+(para-1)*M]] +  // because the values were weighted
                                         factor_w[pollutant[n+(para-1)*M],week[n+(para-1)*M]] + 
                                         factor_c[station[n+(para-1)*M],day[n+(para-1)*M]][pollutant[n+(para-1)*M]] + 
                                         factor_d[day[n+(para-1)*M],pollutant[n+(para-1)*M]]) )),
                        2*cauchy_lpdf(y[n+(para-1)*M] | 0, cauchy_scale[pollutant[n+(para-1)*M]]));
                                         
   }
   // ########################3  
   
for(para in 1:para_cens)
{     
                lp += log_mix(mixture_rho[pollutant_cens[n+(para-1)*M],rho_index_cens[n+(para-1)*M]], gamma_lcdf(censored_threshold | alpha[pollutant_cens[n+(para-1)*M]], (alpha[pollutant_cens[n+(para-1)*M]] / exp(miu[pollutant_cens[n+(para-1)*M]] +factor_q[station_cens[n+(para-1)*M],pollutant_cens[n+(para-1)*M]] + x_cens[n,] * betas[pollutant_cens[n+(para-1)*M]]' + factor_trend[pollutant_cens[n+(para-1)*M]]*((day_cens[n+(para-1)*M]-trend_reference_day)/3652.5)+
                                         factor_h[pollutant_cens[n+(para-1)*M],hour_cens[n+(para-1)*M]] + // have this term because cens data not weighted by hour random.
                                         factor_w[pollutant_cens[n+(para-1)*M],week_cens[n+(para-1)*M]] +
                                         factor_c[station_cens[n+(para-1)*M],day_cens[n+(para-1)*M]][pollutant_cens[n+(para-1)*M]] +
                                         factor_d[day_cens[n+(para-1)*M],pollutant_cens[n+(para-1)*M]]) )),
     2*(cauchy_lcdf(censored_threshold | 0, cauchy_scale[pollutant_cens[n+(para-1)*M]])-0.5));
   }
    // ########################3  
  for(para in 1:para_comp)
   {
     
                lp += H_complete[n+(para-1)*M]*(alpha[pollutant_complete[n+(para-1)*M]])*log(alpha[pollutant_complete[n+(para-1)*M]])
    -H_complete[n+(para-1)*M]*lgamma(alpha[pollutant_complete[n+(para-1)*M]])
    +(alpha[pollutant_complete[n+(para-1)*M]]-1)*sumlog[n+(para-1)*M]
    -H_complete[n+(para-1)*M]*(alpha[pollutant_complete[n+(para-1)*M]])*
        (miu[pollutant_complete[n+(para-1)*M]]+ factor_trend[pollutant_complete[n+(para-1)*M]]*((day_complete[n+(para-1)*M]-trend_reference_day)/3652.5)+
        factor_q[station_complete[n+(para-1)*M],pollutant_complete[n+(para-1)*M]] + x_complete[n,] * betas[pollutant_complete[n+(para-1)*M]]' + 
                                         factor_w[pollutant_complete[n+(para-1)*M],week_complete[n+(para-1)*M]] +
                                         factor_c[station_complete[n+(para-1)*M],day_complete[n+(para-1)*M]][pollutant_complete[n+(para-1)*M]] + 
                                         factor_d[day_complete[n+(para-1)*M],pollutant_complete[n+(para-1)*M]]
        )
    - sumobs[n+(para-1)*M] * (alpha[pollutant_complete[n+(para-1)*M]])/exp(
      miu[pollutant_complete[n+(para-1)*M]]+ factor_trend[pollutant_complete[n+(para-1)*M]]*((day_complete[n+(para-1)*M]-trend_reference_day)/3652.5)+
        factor_q[station_complete[n+(para-1)*M],pollutant_complete[n+(para-1)*M]] + x_complete[n,] * betas[pollutant_complete[n+(para-1)*M]]' + 
                                         factor_w[pollutant_complete[n+(para-1)*M],week_complete[n+(para-1)*M]] +
                                         factor_c[station_complete[n+(para-1)*M],day_complete[n+(para-1)*M]][pollutant_complete[n+(para-1)*M]] + 
                                         factor_d[day_complete[n+(para-1)*M],pollutant_complete[n+(para-1)*M]]); 
                                         
   }                                       
                                         
}
  
  return[lp]';
  
}
}



data {
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of periodical covariates
  matrix[N, K] x; // periodical covariates matrix, a part in X_thp in the model
  real y[N]; // response in the model,y
  int<lower=0> Nh; //=24, as there are total 24 hours
  int<lower=1,upper=Nh> hour[N]; // hour indicator, a column in X_thp
  int<lower=0> Nw; //=7, as there are total 7 weekdays
  int<lower=1,upper=Nw> week[N]; // weekday indicator, a colum in X_thp
  int<lower=0> Nd;// the number of how many days, e.g. two year data usually is 365+365=730, could be 731 as well if 366
  int<lower=1,upper=Nd> day[N];// day indicator, a column in X_thp
  int<lower=1> N_p; // the number of pollutants
  int<lower=1> N_s; // the number of stations
  
  int<lower=1> trend_reference_day; // the reference day for time trend
  
  int<lower=1> pollutant[N]; // pollutant indicator 
  int<lower=1> station[N]; // station indicator
  
  int<lower=1> N_cov; // the number of covariate using for covariance regression
  matrix[Nd, N_cov] x_cov; // covariance predictor matrix
  
  
  int<lower=0> N_cens; // the number of "zero" observations (we deal with the zero censored data only)
  real censored_threshold; // threshold for the censored obs, 0.5 in our data set
  matrix[N_cens, K] x_cens; // corresponds to x in the above
  int<lower=1,upper=Nh> hour_cens[N_cens]; // corresponds to hour in the above
  int<lower=1,upper=Nw> week_cens[N_cens]; // corresponds to week in the above
  int<lower=1,upper=Nd> day_cens[N_cens]; // corresponds to day in the above
  
  int<lower=1> pollutant_cens[N_cens]; // corresponds to pollutant in the above
  int<lower=1> station_cens[N_cens]; // corresponds to station in the above
  int<lower=1> rho_index_cens[N_cens]; // corresponds to station in the above
  
  //for parallise
  int n_shards; // specify how many shards used for pallelize
  int M; // the rounded number of observations for each shard
  
  int<lower=0> N_complete; //
  matrix[N_complete, K] x_complete; // 
  int<lower=1,upper=Nw> week_complete[N_complete]; // 
  int<lower=1,upper=Nd> day_complete[N_complete]; // 
  int<lower=1> pollutant_complete[N_complete]; // 
  int<lower=1> station_complete[N_complete]; // 
  int<lower=1> H_complete[N_complete]; // 
  real sumobs[N_complete];
  real sumlog[N_complete];
  real cauchy_scale[N_p];
  
   matrix[N_p, Nh] factor_h; // h
   int N_rho;
   int<lower=1,upper=N_rho> rho_index[N]; // 
   int sumcomplete[N_p, N_rho];
  int para_hour;
  int para_cens;
  int para_comp;

}

transformed data{
  int xi[n_shards, 6*M*para_hour+6*M*para_cens+5*M*para_comp+7+1+3+1];// as we have 5 integer dataset and other 7 constant data+N_rho+ trend reference day
  real xr[n_shards, (M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+N_p*Nh+1+N_p)];// y and the the K covariates+1 censored_threshold+factor_h,+sumlog,sumobs, x_cens, x_complete
  vector[0] theta[n_shards];
  
  for( i in 1:n_shards)
  {
    xi[i, 1:(M*para_hour)]= station[(1+(i-1)*M*para_hour):(i*M*para_hour)];
    xi[i, (M*para_hour+1):(2*M*para_hour)]= pollutant[(1+(i-1)*M*para_hour):(i*M*para_hour)];
    xi[i, (2*M*para_hour+1):(3*M*para_hour)]= hour[(1+(i-1)*M*para_hour):(i*M*para_hour)];
    xi[i, (3*M*para_hour+1):(4*M*para_hour)]= week[(1+(i-1)*M*para_hour):(i*M*para_hour)];
    xi[i, (4*M*para_hour+1):(5*M*para_hour)]= day[(1+(i-1)*M*para_hour):(i*M*para_hour)];
    xi[i, (5*M*para_hour+1):(6*M*para_hour)]= rho_index[(1+(i-1)*M*para_hour):(i*M*para_hour)];
    
    xi[i, (6*M*para_hour+1):(6*M*para_hour+M*para_cens)]= station_cens[(1+(i-1)*M*para_cens):(i*M*para_cens)];
    xi[i, (6*M*para_hour+M*para_cens+1):(6*M*para_hour+2*M*para_cens)]= pollutant_cens[(1+(i-1)*M*para_cens):(i*M*para_cens)];
    xi[i, (6*M*para_hour+2*M*para_cens+1):(6*M*para_hour+3*M*para_cens)]= hour_cens[(1+(i-1)*M*para_cens):(i*M*para_cens)];
    xi[i, (6*M*para_hour+3*M*para_cens+1):(6*M*para_hour+4*M*para_cens)]= week_cens[(1+(i-1)*M*para_cens):(i*M*para_cens)];
    xi[i, (6*M*para_hour+4*M*para_cens+1):(6*M*para_hour+5*M*para_cens)]= day_cens[(1+(i-1)*M*para_cens):(i*M*para_cens)];
    xi[i, (6*M*para_hour+5*M*para_cens+1):(6*M*para_hour+6*M*para_cens)]= rho_index_cens[(1+(i-1)*M*para_cens):(i*M*para_cens)];
    
    xi[i, (6*M*para_hour+6*M*para_cens+1):(6*M*para_hour+6*M*para_cens+M*para_comp)]= station_complete[(1+(i-1)*M*para_comp):(i*M*para_comp)];
    xi[i, (6*M*para_hour+6*M*para_cens+M*para_comp+1):(6*M*para_hour+6*M*para_cens+2*M*para_comp)]= pollutant_complete[(1+(i-1)*M*para_comp):(i*M*para_comp)];
    xi[i, (6*M*para_hour+6*M*para_cens+2*M*para_comp+1):(6*M*para_hour+6*M*para_cens+3*M*para_comp)]= H_complete[(1+(i-1)*M*para_comp):(i*M*para_comp)];
    xi[i, (6*M*para_hour+6*M*para_cens+3*M*para_comp+1):(6*M*para_hour+6*M*para_cens+4*M*para_comp)]= week_complete[(1+(i-1)*M*para_comp):(i*M*para_comp)];
    xi[i, (6*M*para_hour+6*M*para_cens+4*M*para_comp+1):(6*M*para_hour+6*M*para_cens+5*M*para_comp)]= day_complete[(1+(i-1)*M*para_comp):(i*M*para_comp)];
    
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+1)]=M;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+2)]=K;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+3)]=N_p;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+4)]=N_s;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+5)]=Nh;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+6)]=Nw;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+7)]=Nd;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+7+1)]=N_rho;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+7+2)]=para_hour;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+7+3)]=para_cens;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+7+4)]=para_comp;
    xi[i,((6*M*para_hour+6*M*para_cens+5*M*para_comp)+7+5)]=trend_reference_day;
    
    xr[i, 1:(M*para_hour)]=y[(1+(i-1)*M*para_hour):(i*M*para_hour)];
    xr[i, (M*para_hour+1):(M*para_hour+M*para_comp)]=sumlog[(1+(i-1)*M*para_comp):(i*M*para_comp)];
    xr[i, (M*para_hour+M*para_comp+1):(M*para_hour+2*M*para_comp)]=sumobs[(1+(i-1)*M*para_comp):(i*M*para_comp)];
    xr[i, (M*para_hour+2*M*para_comp+1):(M*para_hour+2*M*para_comp+M*para_hour*K)]=to_array_1d(x[(1+(i-1)*M*para_hour):(i*M*para_hour),]);
    xr[i, (M*para_hour+2*M*para_comp+M*para_hour*K+1):(M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K)]=to_array_1d(x_cens[(1+(i-1)*M*para_cens):(i*M*para_cens),]);
    xr[i, (M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+1):(M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K)]=to_array_1d(x_complete[(1+(i-1)*M*para_comp):(i*M*para_comp),]);
    xr[i, (M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+1):(M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+N_p*Nh)] = to_array_1d(factor_h);
    xr[i, (M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+N_p*Nh+1)] = censored_threshold;
    xr[i, (M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+N_p*Nh+1+1):(M*para_hour+2*M*para_comp+M*para_hour*K+M*para_cens*K+M*para_comp*K+N_p*Nh+1+N_p)] = cauchy_scale;
  }
  

}

parameters {
  vector<lower=0,upper=1>[N_p] arcoef_phi; // the AR coef of daily random effect
 
  matrix[N_p, (Nw-1)] factor_w_pre;// week factor, which weekday it belongs, one colum in X_thp
  matrix[Nd, N_p] factor_d; // daily random effects. =D_tp
  matrix[N_s, N_p] factor_q; // factor for average station, pollutant  = Q_sp
  matrix[Nd, N_p] factor_c[N_s]; // C_stp
  vector[N_p] miu; // mu_p
  vector[N_p-1] factor_trend_raw; // time trend
  matrix[N_p, K] betas; //the regression parameters
  vector<lower=0> [N_p] alpha_pre ; // shape parameter for gamma distribution
  // vector<lower=0, upper=1>[N_p] mixture_rho[N_rho]; // mixture proportion rho in the model
  matrix<lower=0, upper=1>[N_p, N_rho] mixture_rho;
  
   cholesky_factor_corr[N_p] L1; //cholesky factor of covariance L in the model
   cholesky_factor_corr[N_p] G; // G in the model
   vector<lower=0>[N_p] diaggamma; // prior scale
  vector<lower=0>[N_p] tau;// tau in the model
  vector<lower=0>[N_p] kappa; // kappa in the model
   
   //for covariance
     vector[N_p] miu_cov; // b_j in the model, is the intercept
     vector[N_cov] betas_cov[N_p]; // eta in the model, is the regression coef for varying covariance.

}

 transformed parameters
 {
  // save the needed parameters
   matrix[Nd, (N_p-1)] factor_c_save[N_s];
   matrix[(N_p-1),(N_p-1)] L_save;
   matrix[N_p, Nw] factor_w;
   vector[N_p] factor_trend; // time trend
   
   vector[N_p] alpha;
  // 
  matrix[N_p,N_p] L[Nd];
  matrix[N_p,N_p] L_prior1;
  matrix[N_p,N_p] L_prior;
  vector [N_p] temp_arcoef_phi;
  vector[N_rho*N_p+2*N_p + N_s*N_p + N_p*K +  N_p*Nw + Nd*N_p + Nd*N_p*N_s+N_p] total_param;
  
  // use wedesday as baseline
  for(i in 1:2)
  {
    factor_w[,i]=factor_w_pre[,i];
  }
  factor_w[,3]=rep_vector(0,N_p);
    for(i in 4:Nw)
  {
    factor_w[,i]=factor_w_pre[,(i-1)];
  }
  
  
  for(i in 1:N_p)
  {
    alpha[i]=1/((alpha_pre[i])^2);
  }

   factor_trend[1]=factor_trend_raw[1];

  for(i in 1:(N_p-1))
  {
   factor_trend[i+1]=factor_trend_raw[i];
  }

  for(i in 1:Nd)
  {
  L[i]=L1;
     for(j in 1:N_p)
       {
          L[i][N_p,j]=miu_cov[j] + x_cov[i,] * betas_cov[j];
        }
  }
  
  L_prior1=quad_form_diag(tcrossprod(L[1]), tau);
  
  for(i in 1:5)
  {
    temp_arcoef_phi[1]=arcoef_phi[1]^i;
    temp_arcoef_phi[2]=arcoef_phi[2]^i;
    temp_arcoef_phi[3]=arcoef_phi[3]^i; 
    temp_arcoef_phi[4]=arcoef_phi[4]^i;
    L_prior1 += quad_form_diag(tcrossprod(L[1]), tau .* temp_arcoef_phi);
  }
  
  L_prior=(L_prior1+L_prior1')./2;
  
  
  // 
  factor_c_save[,,1] = factor_c[,,1];
  factor_c_save[,,2] = factor_c[,,3];
  factor_c_save[,,3] = factor_c[,,4];
  
  L_save = L[1, :(N_p-1), :(N_p-1)];
  
// the following is used for parallise
  total_param[1:(N_rho*N_p)]=to_vector(mixture_rho);
  total_param[(N_rho*N_p+1):(N_rho*N_p+N_p)]=alpha;
  total_param[(N_rho*N_p+N_p+1):(N_rho*N_p+2*N_p)]=miu;
  
  total_param[(N_rho*N_p+2*N_p+1):(N_rho*N_p+2*N_p+N_s*N_p)]=to_vector(factor_q);
  total_param[(N_rho*N_p+2*N_p+N_s*N_p+1):(N_rho*N_p+2*N_p+N_s*N_p+N_p*K)]=to_vector(betas);
  
  total_param[(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+1):(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+ N_p*Nw)]=to_vector(factor_w);
    
  for(i in 1:N_s)
  {
  total_param[(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+ N_p*Nw+1+(i-1)*Nd*N_p):(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+i*Nd*N_p)]=to_vector(factor_c[i]);
  }
  
  total_param[((N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+N_s*Nd*N_p)+1):(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+N_s*Nd*N_p+Nd*N_p)]=to_vector(factor_d);
  
    total_param[(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+N_s*Nd*N_p+Nd*N_p+1):(N_rho*N_p+2*N_p+N_s*N_p+N_p*K+  N_p*Nw+N_s*Nd*N_p+Nd*N_p+N_p)]=factor_trend;
}

model {
  
  alpha_pre ~ exponential(0.5);
  miu ~ normal(0,10);
  factor_trend_raw ~ normal(0,10);
  
  
  
    for(j in 1:N_p)
    {
     // factor_h[j,] ~ normal(0,10);
     factor_w_pre[j,] ~ normal(0,10);
     mixture_rho[j,] ~ beta(20,1);
    }
  
  
  miu_cov ~ normal(0,1);
  for(i in 1:N_p)
  {
  betas[i] ~ normal(0,10);
  betas_cov[i] ~ normal(0,0.05);
    
  }
  // Priors
  tau ~ exponential(0.5);
  kappa ~ exponential(0.5);
  diaggamma ~ exponential(0.5);
  arcoef_phi ~ uniform(0,1);
  
  L1 ~ lkj_corr_cholesky(1);
  G ~ lkj_corr_cholesky(1);
  
  
  for(i in 1:N_s)
  {
  factor_q[i] ~ multi_normal_cholesky(rep_vector(0, N_p), diag_pre_multiply(kappa, G));
  }
//##
  factor_d[1]' ~ multi_normal(rep_vector(0,N_p), L_prior);
  
  for(i in 2:Nd)
  {
  factor_d[i]' ~ multi_normal_cholesky(diag_matrix(arcoef_phi) * factor_d[(i-1)]', diag_pre_multiply(tau, L[i]));
  }

for(j in 1: N_s)
{
  
  for( i in 1:Nd)
  {
  factor_c[j,i] ~ multi_normal_cholesky(rep_vector(0,N_p), diag_pre_multiply(sqrt(diaggamma .* tau), L[i]));
  }
}
 //##
  
  //########
  // Likelihood
 target += sum(map_rect(lp_reduce, total_param, theta, xr, xi));
 
  for(n in (n_shards*M*para_cens+1):N_cens)
  {
  target +=  log_mix(mixture_rho[pollutant_cens[n],rho_index_cens[n]], gamma_lcdf(censored_threshold | alpha[pollutant_cens[n]], (alpha[pollutant_cens[n]] / exp(miu[pollutant_cens[n]] +factor_q[station_cens[n],pollutant_cens[n]] + x_cens[n,] * betas[pollutant_cens[n]]' +              factor_trend[pollutant_cens[n]]*((day_cens[n]-trend_reference_day)/3652.5)+
                                         factor_h[pollutant_cens[n],hour_cens[n]] +
                                         factor_w[pollutant_cens[n],week_cens[n]] +
                                         factor_c[station_cens[n],day_cens[n]][pollutant_cens[n]] +
                                         factor_d[day_cens[n],pollutant_cens[n]]) )),
     2*(cauchy_lcdf(censored_threshold | 0, cauchy_scale[pollutant_cens[n]])-0.5));
  
  }
             

//########

for(n in (n_shards*M*para_hour+1):N)
  {
     target += log_mix(mixture_rho[pollutant[n],rho_index[n]],
                                 gamma_lpdf(y[n] | alpha[pollutant[n]], (alpha[pollutant[n]] / exp(miu[pollutant[n]]+ factor_q[station                 [n],pollutant[n]] + x[n,] * betas[pollutant[n]]' + 
                                 factor_trend[pollutant[n]]*((day[n]-trend_reference_day)/3652.5)+
                                         // factor_h[pollutant[n],hour[n]] +  // because the values were weighted
                                         factor_w[pollutant[n],week[n]] + 
                                         factor_c[station[n],day[n]][pollutant[n]] + 
                                         factor_d[day[n],pollutant[n]]) )),
                        2*cauchy_lpdf(y[n] | 0, cauchy_scale[pollutant[n]]));
                                         
  }                                     

//########
 for(n in (n_shards*M*para_comp+1):N_complete)
  {
   target += 
    H_complete[n]*alpha[pollutant_complete[n]]*log(alpha[pollutant_complete[n]])
    -H_complete[n]*lgamma(alpha[pollutant_complete[n]])
    +(alpha[pollutant_complete[n]]-1)*sumlog[n]
    -H_complete[n]*alpha[pollutant_complete[n]]*
        (miu[pollutant_complete[n]]+ factor_trend[pollutant_complete[n]]*((day_complete[n]-trend_reference_day)/3652.5)+
        factor_q[station_complete[n],pollutant_complete[n]] + x_complete[n,] * betas[pollutant_complete[n]]' + 
                                         factor_w[pollutant_complete[n],week_complete[n]] +
                                         factor_c[station_complete[n],day_complete[n]][pollutant_complete[n]] + 
                                         factor_d[day_complete[n],pollutant_complete[n]]
        )
    - sumobs[n] * alpha[pollutant_complete[n]]/exp(
      miu[pollutant_complete[n]]+ factor_trend[pollutant_complete[n]]*((day_complete[n]-trend_reference_day)/3652.5)+
        factor_q[station_complete[n],pollutant_complete[n]] + x_complete[n,] * betas[pollutant_complete[n]]' + 
                                         factor_w[pollutant_complete[n],week_complete[n]] +
                                         factor_c[station_complete[n],day_complete[n]][pollutant_complete[n]] + 
                                         factor_d[day_complete[n],pollutant_complete[n]]
    ); 
    

}

//#####
for(i in 1:N_p)
{
  for(j in 1:N_rho)
  {
    target +=  binomial_lpmf(sumcomplete[i,j] | sumcomplete[i,j], mixture_rho[i,j]);
  }
}

}





