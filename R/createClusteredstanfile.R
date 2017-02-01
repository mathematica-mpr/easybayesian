#' @title Creates Clustered stan file

createClusteredStanfile <- function(){
  "
    data {
    int<lower=0> N;    // number of data items
    int<lower=0> K;    // number of predictors
    int<lower=0> J;    // numer of clusters
    vector[N] y;       // outcome vector
    vector[K] x[N];    // predictor matrix
    int cluster[N];    // numeric cluster identifier
    }

    parameters {
    real alpha;                   // intercept
    vector[K] beta;               // coefficients for predictors
    real<lower=0,upper=10> sigma; // error sd
    vector[J] b_raw;              // cluster random effects
    real<lower=0,upper=10> tau;   // cluster sd
    }

    transformed parameters{
    vector[J] b;
    b = b_raw * tau;
    }

    model {
    real yHat[N];
    for(i in 1:N){
    yHat[i] = alpha + dot_product(x[i], beta) + b[cluster[i]];
    }
    y ~ normal(yHat, sigma); // likelihood
    b_raw ~ normal(0, 1);
    }

    "
}
