functions { 
  real quantile(vector x, real p){
    int n;            // length of vector x
    real index;       // integer index of p
    int lo;           // lower integer cap of the index
    int hi;           // higher integer cap of the index
    real h;           // generated weight between the lo and hi
    real qs;          // weighted average of x[lo] and x[hi]
    n = num_elements(x);           
    index = 1 + (n - 1)*p;             
    lo = 1; 
    while ((lo + 1) < index) 
      lo = lo + 1; 
    hi = lo + 1; 
    h = index - lo; 
    qs = (1 - h)*sort_asc(x)[lo] + h*sort_asc(x)[hi];     
    return qs;                
  }
  
  // Find the number of elements in the vector x that equal real number y
 int num_matches(vector x, real y) {
  int n = 0;
  for (i in 1:rows(x))
    if (x[i] == y)
      n += 1;
  return n;
}


 // Find the indexes of the elements in the vector x that equal real number y
 
 int[] which_equal(vector x, real y) {
  int match_positions[num_matches(x, y)];
  int pos = 1;
  for (i in 1:rows(x)) {
    if (x[i] == y) {
      match_positions[pos] = i;
      pos += 1;
    }
  }
  return match_positions;
}

 // Find the mean of the elements in the vector output such that the corresponding elements in the vector input are equal to x
 
 real which_mean(vector input, real x, vector output) {
   real sum_output=0;
   real mean_output;
   for (i in 1:rows(input)) {
     if (input[i]==x)
      sum_output = sum_output + output[i];
   }
   mean_output = sum_output/num_matches(input,x);
   return mean_output;
 }

 // Find the median of the elements in the vector output such that the corresponding elements in the vector input are equal to x
 
 real which_median(vector input, real x, vector output) {
   vector[num_matches(input,x)] relevant_output;
   real median_output;
   int j=1;
   for (i in 1:rows(input)) {
     if (input[i]==x) {
      relevant_output[j] = output[i];
      j = j+1;
     }
   }
   median_output = quantile(relevant_output,0.50);
   return median_output;
 }
 // Find the number of elements in the vector input that are greater or equal than x and strictly smaller than y

 int num_between(vector input, real x, real y) {
  int n = 0;
  for (i in 1:rows(input)) {
    if (input[i] >= x) 
      if (input[i] < y)
       n += 1;
    }
  return n;
}
 // Find the mean of the elements of the vector output such that the corresponding elements in the vector input that are greater or
 // equal than x and strictly smaller than y
 
 real mean_between(vector input, real x, real y, vector output) {
   real sum_output=0;
   real mean_output;
     for (i in 1:rows(input)) {
      if (input[i] >= x) 
      if (input[i] < y)
        sum_output = sum_output + output[i];
  }
  mean_output = sum_output/num_between(input,x,y);
  return mean_output;
 }
 
  // Find the number of elements in a 3 dimensional cell of covariates

 int num_between_cell(vector input_1, vector input_2, vector input_3, real x_1, 
 real y_1, real x_2, real y_2, real x_3, real y_3) {
  int n = 0;
  for (i in 1:rows(input_1)) {
    if (input_1[i] >= x_1) 
      if (input_1[i] < y_1)
        if (input_2[i] >= x_2) 
          if (input_2[i] < y_2)
            if (input_3[i] >= x_3) 
              if (input_3[i] < y_3)
       n += 1;
    }
  return n;
}
 // Find the mean of the elements of the vector output such that the corresponding elements in the vector input
 // belong to a certain cell
 
 real mean_between_cell(vector input_1, vector input_2, vector input_3, real x_1, 
 real y_1, real x_2, real y_2, real x_3, real y_3, vector output) {
   real sum_output=0;
   real mean_output;
     for (i in 1:rows(input_1)) {
    if (input_1[i] >= x_1) 
      if (input_1[i] < y_1)
        if (input_2[i] >= x_2) 
          if (input_2[i] < y_2)
            if (input_3[i] >= x_3) 
              if (input_3[i] < y_3)
        sum_output = sum_output + output[i];
  }
  mean_output = sum_output/num_between_cell(input_1, input_2, input_3, x_1, y_1, x_2,
  y_2, x_3, y_3);
  return mean_output;
 }

 int num_exact_cell(vector input_1, vector input_2, vector input_3, real x_1, 
 real x_2, real x_3) {
  int n = 0;
  for (i in 1:rows(input_1)) {
    if (input_1[i] == x_1) 
        if (input_2[i] == x_2) 
            if (input_3[i] == x_3) 
       n += 1;
    }
  return n;
}

real mean_exact_cell(vector input_1, vector input_2, vector input_3, real x_1, 
 real x_2, real x_3, vector output) {
   real sum_output=0;
   real mean_output;
     for (i in 1:rows(input_1)) {
    if (input_1[i] == x_1) 
        if (input_2[i] == x_2) 
            if (input_3[i] == x_3) 
        sum_output = sum_output + output[i];
  }
  mean_output = sum_output/num_exact_cell(input_1, input_2, input_3, x_1, x_2, x_3);
  return mean_output;
 }

 // Find the median of the elements of the vector output such that the corresponding elements in the vector input
 // belong to a certain cell
 
 real median_between_cell(vector input_1, vector input_2, vector input_3, real x_1, 
 real y_1, real x_2, real y_2, real x_3, real y_3, vector output) {
   vector[num_between_cell(input_1, input_2, input_3, x_1, y_1, x_2,
  y_2, x_3, y_3)] relevant_output;
   real median_output;
   int j = 1;
     for (i in 1:rows(input_1)) {
    if (input_1[i] == x_1) 
        if (input_2[i] == x_2) 
            if (input_3[i] == x_3) {
                  relevant_output[j] = output[i];
                  j = j+1;
     }
 }   
   median_output = quantile(relevant_output,0.50);
   return median_output;
  }

 

 real median_exact_cell(vector input_1, vector input_2, vector input_3, real x_1, real x_2, real x_3, vector output) {
   vector[num_exact_cell(input_1, input_2, input_3, x_1, x_2, x_3)] relevant_output;
   real median_output;
   int j = 1;
     for (i in 1:rows(input_1)) {
    if (input_1[i] == x_1) 
        if (input_2[i] == x_2) 
            if (input_3[i] == x_3) {
                  relevant_output[j] = output[i];
                  j = j+1;
     }
  }
   median_output = quantile(relevant_output,0.50);
   return median_output;
 }

}

data {
  int<lower=0> N;              // num individuals
  int<lower=1> K;              // num ind predictors
  int<lower=1> J;              // num groups
  int<lower=1> L;              // num group predictors
  int<lower=1,upper=J> g[N];  // group for individual
  matrix[N, K] x;             // individual predictors
  matrix[J,L] u;                 // group predictors
  vector[N] y;                 // outcomes
  vector[N] p;                // percentiles
  vector[N] w;                      // treatment assigned
  vector[N] girl;             
  vector[N] age_quartile;
  vector[N] etp_teacher;
  vector[N] sbm;
  vector[N] visit_quartile;
  vector[N] yrstaught_quartile;
  vector[N] percentile_quartile;
  matrix[N,9] zone;
}

transformed data{
  
  matrix[L,J] u_transp; 
  matrix[N, K] Q_ast;
  matrix[K, K] R_ast;
  
  // thin and scale the QR decomposition
  
  Q_ast = qr_thin_Q(x) * sqrt(N - 1);
  R_ast = qr_thin_R(x) / sqrt(N - 1);
  
  // group predictors transposed
  
  u_transp = u';
}

parameters {
  
  matrix[K,J] z;               // this will be the basis for the beta's
                              // transformation
  vector[J] z_2;
  
  matrix[K,L] gamma;              // school predictors coefficients
  vector<lower=0,upper=pi()/2>[K] tau_unif;  // heterskedasticity component
                                             // for the coefficients
  real<lower=0,upper=pi()/2> sigma_alpha_unif;      // SE for the constant
  
  vector[L] eta;
  
  real effect;                      // super-population average treatment effect
  real<lower=0,upper=pi()/2> sigma_t_unif;        // residual SD for the treated
  real<lower=0,upper=pi()/2> sigma_c_unif;        // residual SD for the control
  
  cholesky_factor_corr[K] L_Omega;  // corr matrix for the slopes
 
}

transformed parameters{
   
   vector[J] alpha;                 // vector of school-specific intercepts
   vector[J] c;                     // intercepts' expected value

  // All SE's have half-Cauchy priors, but we sample them from a Uniform and
  // then take the tangent as it is computationally more efficient, note that
  // if X is Uniform, then Y is a std half-Cauchy
  
   vector<lower=0>[K] tau = tan(tau_unif);
   real<lower=0> sigma_alpha = tan(sigma_alpha_unif);
   real<lower=0> sigma_t = tan(sigma_t_unif);
   real<lower=0> sigma_c = tan(sigma_c_unif);
   
  // School specific slopes, since it is the transformation of a 
  // multivariate normal, we do not need to give a prior to beta, but rather
  // to z and to the variance component (tau) and the correlation component
  // of the Cholensky decomposition (L_Omega). On top of that, we utilize the
  // QR-reparametrized theta = R_ast * beta
   
   matrix[K,J] theta;            // vector of school-specific coefficients
   matrix[K,K] L_Sigma; 
  
  
   
  
  // Here we define beta, not that since it is the transformation of a 
  // multivariate normal, we do not need to give a prior to beta, but rather
  // to z and to the variance component (tau) and the correlation component
  // of the Cholensky decomposition (L_Omega)
  
    // This is theta's variance-covariance matrix
  
  L_Sigma = cholesky_decompose(quad_form(multiply_lower_tri_self_transpose( diag_pre_multiply(tau, L_Omega )), R_ast'));
  
  theta = R_ast * (gamma * u_transp) + L_Sigma * z;
  
  // intercepts' expected value
  
  for (j in 1:J) {
    c[j] = eta' * u_transp[ ,j];
  }
  
  // School specific intercepts
  
  alpha = c + sigma_alpha * z_2;
}

model {
  
 
  vector[N] m;
  vector[N] d;
  vector[N] uno;
  
  
  tau_unif ~ uniform(0,pi()/2);
  L_Omega ~ lkj_corr_cholesky(2);
  
  to_vector(z) ~ std_normal();
  to_vector(gamma) ~ std_normal();
  eta ~ std_normal();
  z_2 ~ std_normal();
  
  
  
  effect ~ normal(0,2);
  sigma_c_unif ~ uniform(0,pi()/2);          
  sigma_t_unif ~ uniform(0,pi()/2);
  sigma_alpha_unif ~ uniform(0,pi()/2);
  
  // Here we model the likelihood of our outcome variable y
  
    // Its expected value
    
  for (n in 1:N) {
    m[n] = alpha[g[n]] + Q_ast[n, ] * theta[, g[n]] + effect*w[n];
  }
  
    // Its variance
    
  uno = rep_vector(1, N);
  d = sigma_t*w + sigma_c*(uno-w);
  
  y ~ normal(m, d);

}

generated quantities{
  
  real effect_fs;                      // finite-sample average treatment effect
  real effect_qte25;                   // quantile treatment effect at p = 0.25
  real effect_qte50;                   // quantile treatment effect at p = 0.50
  real effect_qte75;                   // quantile treatment effect at p = 0.75
  vector[2] y_potential[N];
  vector[2] mu_treat[N];
  real y0[N];                       // potential outcome if W = 0
  real y1[N];                       // potential outcome if W = 1
  real effect_unit[N];             // unit-level treatment effect
  int lucky_25;
  int lucky_50;
  int lucky_75;
  real effect_pre_qte25;
  real effect_pre_qte50;
  real effect_pre_qte75;
  
  real effect_stdmark_qte1_mean;
  real effect_stdmark_qte2_mean;
  real effect_stdmark_qte3_mean;
  real effect_stdmark_qte4_mean;
  
  real effect_girl_mean;
  real effect_boy_mean;
  
  real effect_age_qte1_mean;
  real effect_age_qte2_mean;
  real effect_age_qte3_mean;
  real effect_age_qte4_mean;
  
  real effect_etp_mean;
  real effect_noetp_mean;
  
  real effect_sbm_mean;
  real effect_nosbm_mean;
  
  real effect_visit_qte1_mean;
  real effect_visit_qte2_mean;
  real effect_visit_qte3_mean;
  real effect_visit_qte4_mean;
  
  real effect_yrstaught_qte1_mean;
  real effect_yrstaught_qte2_mean;
  real effect_yrstaught_qte3_mean;
  real effect_yrstaught_qte4_mean;
  
  vector[9] effect_zone_mean;
  
  matrix[4,4] girls_mean_effect;
  matrix[4,4] girls_median_effect;
  
  matrix[4,4] boys_mean_effect;
  matrix[4,4] boys_median_effect;  
    
  real rho = 0.5;
  matrix[2,2] V = [[1, rho], [rho, 1]];
  matrix[2,2] V_l;
  vector[2] s = [sigma_c, sigma_t]';

  V_l = cholesky_decompose(quad_form_diag(V,s));
  
  for(n in 1:N){
    mu_treat[n,1] = alpha[g[n]] + Q_ast[n, ] * theta[, g[n]];            
    mu_treat[n,2] = alpha[g[n]] + Q_ast[n, ] * theta[, g[n]] + effect;
    y_potential[n] = multi_normal_cholesky_rng(mu_treat[n], V_l);
    if(w[n] == 1){                
      y0[n] = y_potential[n,1];
      y1[n] = y[n];
    }else{                        
      y0[n] = y[n];       
      y1[n] = y_potential[n,2];
    }
    effect_unit[n] = y1[n] - y0[n];
  }
  
  effect_fs = mean(effect_unit);
  effect_qte25 = quantile(to_vector(effect_unit), 0.25); 
  effect_qte50 = quantile(to_vector(effect_unit), 0.50); 
  effect_qte75 = quantile(to_vector(effect_unit), 0.75); 
  
  // lucky_25 = mean(which_equal(p, y = quantile(p, 0.75)));
  
  lucky_25 = min(which_equal(p, 25.00000));
  effect_pre_qte25=effect_unit[lucky_25];
  
  lucky_50 = min(which_equal(p, 50.00000));
  effect_pre_qte50=effect_unit[lucky_50];
  
  lucky_75 = min(which_equal(p, 75.00000));
  effect_pre_qte75=effect_unit[lucky_75];
  
  effect_stdmark_qte1_mean = mean_between(p,0,25.00000, to_vector(effect_unit));
  effect_stdmark_qte2_mean = mean_between(p,25.00000,50.00000,to_vector(effect_unit));
  effect_stdmark_qte3_mean = mean_between(p,50.00000,75.00000,to_vector(effect_unit));
  effect_stdmark_qte4_mean = mean_between(p,75.00000,100.00001,to_vector(effect_unit));
  
  effect_girl_mean = which_mean(girl,1,to_vector(effect_unit));
  effect_boy_mean = which_mean(girl,0,to_vector(effect_unit));
  
  effect_age_qte1_mean = which_mean(age_quartile, 1, to_vector(effect_unit));
  effect_age_qte2_mean = which_mean(age_quartile, 2, to_vector(effect_unit));
  effect_age_qte3_mean = which_mean(age_quartile, 3, to_vector(effect_unit));
  effect_age_qte4_mean = which_mean(age_quartile, 4, to_vector(effect_unit));
  
  effect_etp_mean = which_mean(etp_teacher,1,to_vector(effect_unit));
  effect_noetp_mean = which_mean(etp_teacher,0,to_vector(effect_unit));
  
  effect_sbm_mean = which_mean(sbm,1,to_vector(effect_unit));
  effect_nosbm_mean = which_mean(sbm,0,to_vector(effect_unit));
  
  effect_visit_qte1_mean = which_mean(visit_quartile, 1, to_vector(effect_unit));
  effect_visit_qte2_mean = which_mean(visit_quartile, 2, to_vector(effect_unit));
  effect_visit_qte3_mean = which_mean(visit_quartile, 3, to_vector(effect_unit));
  effect_visit_qte4_mean = which_mean(visit_quartile, 4, to_vector(effect_unit));
  
  effect_yrstaught_qte1_mean = which_mean(yrstaught_quartile, 1, to_vector(effect_unit));
  effect_yrstaught_qte2_mean = which_mean(yrstaught_quartile, 2, to_vector(effect_unit));
  effect_yrstaught_qte3_mean = which_mean(yrstaught_quartile, 3, to_vector(effect_unit));
  effect_yrstaught_qte4_mean = which_mean(yrstaught_quartile, 4, to_vector(effect_unit));
  
  for (i in 1:9) {
    effect_zone_mean[i] = which_mean(zone[,i],1,to_vector(effect_unit));
  }
  

  for (i in 1:4) {
    for (j in 1:4) {
      girls_mean_effect[i,j] = mean_exact_cell(girl, percentile_quartile, age_quartile, 1, i, j, to_vector(effect_unit));
      girls_median_effect[i,j] = median_exact_cell(girl, percentile_quartile, age_quartile, 1, i, j,  to_vector(effect_unit));
      boys_mean_effect[i,j] = mean_exact_cell(girl, percentile_quartile, age_quartile, 0, i, j, to_vector(effect_unit));
      boys_median_effect[i,j] = median_exact_cell(girl, percentile_quartile, age_quartile, 0, i, j, to_vector(effect_unit));
  }
}

}


