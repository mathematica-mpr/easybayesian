#' @title STAN linear model
#' @export
#' @import dplyr
#' @importFrom texreg createTexreg
#' @import rstan
#' @param formula A symbolic description of the model to be fitted.
#' @param cluster An optional parameter that indicates the cluster variable.
#' @param data A data frame containing the variables in the model.
#' @param credible An optinal parameter to indicate the width of the credible interval.
#' @param chains A positive integer specifying number of chains; defaults to 4.
#' @param iter A positive integer specifying how many iterations for each chain (including warmup). The default is 2000.
#' @param thin A positive integer specifying the period for saving sample; defaults to 1.
#' @return A list containing 6 objects:
#' \code{tbl} the regression table,
#' \code{posteriorSamples} a list containing the posterior samples for beta and alpha,
#' \code{fit} the output from \code{\link[rstan]{stan}},
#' \code{credible} the credible interval for the parameters,
#' \code{custom.columns}, and
#' \code{traceplots}
#' @seealso \code{\link{regtbl}} for regression table,
#' \code{\link{posteriorplot}} for plotting the posterior distribution,
#' \code{\link{interpret}} for interpreting the results,
#' \code{\link{bayesiandashboard}} \code{\link{shiny}} dashboard.
#' @examples
#' set.seed(9782)
#' library(dplyr)
#' N <- 1000
#' df1 <- data.frame(
#'   x1 = rnorm(n = N, mean = 10, sd = 3),
#'   x2 = runif(n = N, min = 0, max = 10),
#'   c = sample(LETTERS, size = N, replace = T)) %>%
#'   mutate(Tr = ifelse(c %in% c("A","E","I","O","U"),
#'                      yes = 1, no = 0)) %>%
#'   mutate(y = 0.5*x1 + 0.75*x2 + 0.5*Tr + rnorm(N,0,1))
#' fit1 <- stanlm(formula = y ~ x1 + x2 + Tr, cluster = "c", data = df1)
#' regtbl(fit1, type = "html")
#' posteriorplot(model = fit1, parameter = "Tr", cutoff = 0.4)
#' @section Vignette:
#' For more details check \code{vignette("stanlm", package = "easybayesian")}

stanlm <- function(formula, cluster=NULL, data, credible = .95,
                   chains = 4, iter = 2000, thin = 1, 
                   adapt_delta = 0.8){
  #browser()
  data <- as.data.frame(data)
  clustered <- !is.null(cluster)
  if(clustered){
    modelString <- createClusteredStanfile()
  }else{
    modelString <- createStanfile()
  }

  outcome <- as.character(formula)[2]
  covariates <- attr(terms(formula), 'term.labels')
  # if(clustered) cluster <- as.character(arguments$cluster)

  ####
  K <- length(covariates)
  # stan can't handle missing data
  if(clustered){
    df1 <- data[, c(outcome, covariates, cluster)] %>%
      filter(complete.cases(.))
    df1[,cluster] <- as.numeric(as.factor(df1[,cluster]))
  }else{
    df1 <- data[, c(outcome, covariates)] %>%
      filter(complete.cases(.))
  }
  #
  meanY <- mean(df1[,outcome])
  sdY <- sd(df1[,outcome])
  if(is.null(dim(df1[,covariates]))){
    meanX <- mean(df1[,covariates])
    sdX <- sd(df1[,covariates])
  }else{
    meanX <- df1[,covariates] %>% summarise_each(funs(mean)) %>% as.numeric()
    sdX <- df1[,covariates] %>% summarise_each(funs(sd)) %>% as.numeric()
  }

  df1Rescaled <- scale(df1) ### CHANGED FROM as.data.frame(scale(df1))
  # put data in a list for stan
  if(clustered){
    data2 <- list(N = nrow(df1Rescaled),
                  K = K,
                  J = length(unique(df1[,cluster])),
                  y = df1Rescaled[,outcome],
                  x = as.matrix(df1Rescaled[,covariates]),
                  cluster = as.numeric(factor(df1Rescaled[,cluster])))
    # previously cluster = factor(df1Rescaled[,cluster])), but this failed in production

    # compile the model and run the sampler
    fit <- stan(model_code = modelString, data=data2,
                chains = chains, iter = iter, thin = thin,
                cores=min(parallel::detectCores(), 4), seed = 9782,
                control = list(adapt_delta = adapt_delta))

  }else{
    data2 <- list(N = nrow(df1Rescaled),
                  K = K,
                  y = df1Rescaled[,outcome],
                  x = as.matrix(df1Rescaled[,covariates]))
    # compile the model and run the sampler
    fit <- stan(model_code = modelString, data=data2,
                chains = chains, iter = iter, thin = thin,
                cores=min(parallel::detectCores(), 4), seed = 9782)
  }

  if(length(fit@model_pars) == 0) stop('No outcome and/or predictor variables were found :(') ### CHANGED FROM Something went wrong and the fit object is empty :(

  posteriorSamplesBeta <- t(t(rstan::extract(fit, pars='beta')[[1]]) * sdY/sdX)

  posteriorSamplesAlpha <- rstan::extract(fit, pars='alpha')[[1]] * sdY -
    rowSums(t(t(rstan::extract(fit, pars='beta')[[1]]) * sdY/sdX*meanX)) +
    meanY

  if(K==1){
    betas <- posteriorSamplesBeta %>% as.data.frame() %>%
      summarise_each(funs(mean, sd)) %>%
      as.data.frame()
    names(betas) <- c("coef", "SD")
  }else{
    betas <- posteriorSamplesBeta %>% as.data.frame() %>%
      summarise_each(funs(mean, sd)) %>%
      tidyr::gather(variable, value) %>%
      tidyr::separate(variable, c("var", "stat"), sep = "\\_") %>%
      tidyr::spread(var, value) %>% select(-stat) %>%
      t() %>% as.data.frame()
    names(betas) <- c("coef", "SD")
    betas <- betas[paste0("V", 1:nrow(betas)),]
  }



  ci <- data.frame(t(sapply(1:K, function(j)credibleInterval(posteriorSamplesBeta[,j], credible))))
  names(ci) <- c("lb", "ub")
  betas <- bind_cols(betas, ci)

  alpha <- data.frame(coef = mean(posteriorSamplesAlpha),
                      SD = sd(posteriorSamplesAlpha))

  ci <- credibleInterval(posteriorSamplesAlpha, credible)
  ci <- data.frame(lb = ci[[1]], ub = ci[[2]])
  alpha <- bind_cols(alpha, ci)

  coef <- bind_rows(betas, alpha)
  rownames(coef) <- c(covariates, "Constant")

  log_posterior_n_Rhat <- summary(fit)$summary[,"Rhat"]["lp__"]
  log_posterior_n_eff <- summary(fit)$summary[,"n_eff"]["lp__"]

  Rhat <- "R&#770"
  n_eff <- paste0("N","<sub>eff</sub>")
  R_lp <- paste0(Rhat, " log-posterior")
  N_lp <- paste0(n_eff, " log-posterior")


  if(clustered){
    model <- createTexreg(
      coef.names = rownames(coef),
      coef = coef$coef,
      se = coef$SD,
      ci.low = coef$lb,
      ci.up = coef$ub,
      model.name = "Clustered Stan",
      gof.names = c("N", "Clusters ",
                    R_lp, N_lp),
      gof = c(nrow(df1),
              length(unique(df1[,cluster])),
              log_posterior_n_Rhat,
              log_posterior_n_eff),
      gof.decimal = c(FALSE, FALSE, TRUE, FALSE)
    )
  }else{
    model <- createTexreg(
      coef.names = rownames(coef),
      coef = coef$coef,
      se = coef$SD,
      ci.low = coef$lb,
      ci.up = coef$ub,
      model.name = "Unclustered Stan",
      gof.names = c("N", R_lp, N_lp),
      gof = c(nrow(df1), log_posterior_n_Rhat, log_posterior_n_eff),
      gof.decimal = c(FALSE, TRUE, FALSE)
    )
  }
  posteriorSamplesBeta <- as.data.frame(posteriorSamplesBeta)
  names(posteriorSamplesBeta) <- covariates
  Rhat <- sprintf("%.2f", round(summary(fit)$summary[,"Rhat"][c(2:(K+1),1)],2))
  n_eff <- round(summary(fit)$summary[,"n_eff"][c(2:(K+1),1)],0)
  custom.columns <- list(Rhat=Rhat, n_eff=n_eff)
  traceplots <- traceplot(fit, pars = c(names(fit)[1:(K+1)], "sigma", "lp__"))

  output <- list(tbl = model,
                 posteriorSamples = list(posteriorSamplesBeta = posteriorSamplesBeta,
                                                      posteriorSamplesAlpha = posteriorSamplesAlpha),
                 fit = fit,
                 credible = credible,
                 custom.columns = custom.columns,
                 traceplots = traceplots)
  return(output)
}
