#' @title Creates probit stan file

createStanprobitfile <- function(){
  if(!file.exists('probit.stan')){
    modelString <- "
    data {
    int<lower=0> N;    // number of data items
    int<lower=0> K;    // number of predictors
    int<lower=0,upper=1> y[N];       // outcome vector
    vector[K] x[N];    // predictor matrix
    }
    
    parameters {
    real alpha;                   // intercept
    vector[K] beta;               // coefficients for predictors
    }
    
    model {
    real yHat[N];
    for(i in 1:N){
    yHat[i] <- alpha + dot_product(x[i], beta);
    }
    for(i in 1:N){
    y[i] ~ bernoulli(Phi_approx(yHat[i])); // likelihood
    }
    
    }
    "
    writeLines(modelString, con="probit.stan")
    }
  }