#' @title Creates stan file
#' @export

createStanfile <- function(){
  "data {
    int<lower=0> N;    // number of data items
    int<lower=0> K;    // number of predictors
    vector[N] y;       // outcome vector
    vector[K] x[N];    // predictor matrix
  }

  parameters {
    real alpha;                   // intercept
    vector[K] beta;               // coefficients for predictors
    real<lower=0> sigma; // error sd
  }

  model {
    real yHat[N];
    for(i in 1:N){
      yHat[i] = alpha + dot_product(x[i], beta); 
    }
    y ~ normal(yHat, sigma); // likelihood
    beta ~ normal(0,1); 
    }

    "
}
