functions {
  matrix pred_theta(matrix Xp, matrix Zp, int p, matrix beta, matrix u){
  int D1 = rows(Xp);
  real num1[D1, p];
  real den1[D1];
  matrix[D1,p] theta_p;
  matrix[D1,p] tasa_pred;
  
  for(d in 1:D1){
    num1[d, 1] = 1;
    num1[d, 2] = exp(Xp[d, ] * beta[1, ]' + Zp[d, ] * u[1, ]') ;
    num1[d, 3] = exp(Xp[d, ] * beta[2, ]' + Zp[d, ] * u[2, ]') ;
    
    den1[d] = sum(num1[d, ]);
  }
  
  for(d in 1:D1){
    for(i in 2:p){
    theta_p[d, i] = num1[d, i]/den1[d];
    }
    theta_p[d, 1] = 1/den1[d];
   }


for(d in 1:D1){
    tasa_pred[d, 1] = theta_p[d,1]/(theta_p[d,1] + theta_p[d,2]);// TD
    tasa_pred[d, 2] = theta_p[d,2];                              // TO
    tasa_pred[d, 3] = tasa_pred[d, 2]/(1- tasa_pred[d, 1]);               // TP
    }

  return tasa_pred  ;
  }
  
}

data {
  int<lower=1> D;    // número de postestrto 
  int<lower=1> D1;   // número de dominios por predesir 
  int<lower=1> P;    // categorías
  int<lower=1> K;  // cantidad de regresores
  int<lower=1> Kz; // cantidad de regresores en Z
  int y[D, P];       // matriz de datos
  matrix[D, K] X; // matriz de covariables
  matrix[D, Kz] Z; // matriz de covariables
  matrix[D1, K] Xp; // matriz de covariables
  matrix[D1, Kz] Zp; // matriz de covariables
}
  

parameters {
  matrix[P-1, K] beta;// matriz de parámetros 
  vector<lower=0>[P-1] sigma2_u;       // random effects standard deviations
  matrix[P-1, Kz] u; 
}

transformed parameters {
  simplex[P] theta[D];// vector de parámetros;
  real num[D, P];
  real den[D];
  vector<lower=0>[P-1] sigma_u;       // random effects standard deviations
  sigma_u = sqrt(sigma2_u); 
   

  for(d in 1:D){
    num[d, 1] = 1;
    num[d, 2] = exp(X[d, ] * beta[1, ]' +  Z[d, ] * u[1, ]') ;
    num[d, 3] = exp(X[d, ] * beta[2, ]' +  Z[d, ] * u[2, ]') ;
    
    den[d] = sum(num[d, ]);
  }
  
  for(d in 1:D){
    for(p in 2:P){
    theta[d, p] = num[d, p]/den[d];
    }
    theta[d, 1] = 1/den[d];
  }
}

model {
 to_vector(sigma2_u) ~  inv_gamma(0.0001, 0.0001);
 
 for(p in 1:(P-1)){
   u[p , ] ~ normal(0, sigma_u[p]);
 }
 
 to_vector(beta) ~ normal(0, 100);
  
  for(d in 1:D){
    target += multinomial_lpmf(y[d, ] | theta[d, ]); 
  }
}

  
generated quantities {
  matrix[D1,P] tasa_pred;
  tasa_pred = pred_theta(Xp,Zp,P, beta, u) ; 
}
