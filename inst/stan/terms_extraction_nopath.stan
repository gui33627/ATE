

data {
  int<lower=1> N; // number of records
  int<lower=1> M; // number of terms
  int<lower=1> N_e; // number of events
  int<lower=1> N_s; // number of sites
  
  
  real<lower=0> tau; // standard deviation of event terms
  real<lower=0> phi_S2S; // standard deviation of site terms
  
  
  vector[N] r;  // calculated total residuals
  array[N, M] int mat_ids; // matrix of indices for events, sites, and paths of each record
  // matrix[N,M] mat_ids; // matrix of indices for events, sites, and paths of each record
}

parameters {
  vector[N_e] delta_B;
  vector[N_s] delta_S2S;
  
  
  real<lower=0> phi_SS;
}


model {
  for(j in 1:N_e) {
 	delta_B[j] ~ normal(0, tau);
  }
  for(j in 1:N_s) {
 	delta_S2S[j] ~ normal(0, phi_S2S);
  }
  
  
  real tmp_res_mean;
  for(i in 1:N) {
	tmp_res_mean = 0;
	tmp_res_mean += delta_B[mat_ids[i,1]];
	tmp_res_mean += delta_S2S[mat_ids[i,2]];
	r[i] ~ normal(tmp_res_mean, phi_SS);
  }
}
