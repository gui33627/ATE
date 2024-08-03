

data {
  int<lower=1> N; // number of records
  int<lower=1> M; // number of terms
  int<lower=1> N_e; // number of events
  int<lower=1> N_s; // number of sites
  
  
  real<lower=0> eta_e_sd; // standard deviation of event terms
  real<lower=0> eta_s_sd; // standard deviation of site terms
  
  
  vector[N] tot_res;  // calculated total residuals
  array[N, M] int mat_ids; // matrix of indices for events, sites, and paths of each record
  // matrix[N,M] mat_ids; // matrix of indices for events, sites, and paths of each record
}

parameters {
  vector[N_e] eta_e;
  vector[N_s] eta_s;
  
  
  real<lower=0> eps_sd;
}


model {
  for(j in 1:N_e) {
 	eta_e[j] ~ normal(0, eta_e_sd);
  }
  for(j in 1:N_s) {
 	eta_s[j] ~ normal(0, eta_s_sd);
  }
  
  
  real tmp_res_mean;
  for(i in 1:N) {
	tmp_res_mean = 0;
	tmp_res_mean += eta_e[mat_ids[i,1]];
	tmp_res_mean += eta_s[mat_ids[i,2]];
	tot_res[i] ~ normal(tmp_res_mean, eps_sd);
  }
}
