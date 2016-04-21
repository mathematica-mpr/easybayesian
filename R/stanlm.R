#' @title STAN linear model
#' @export
#' @import dplyr
#' @importFrom texreg createTexreg
#' @import rstan

stanlm <- function(formula, cluster=NULL, data, credible = .95){
  #browser()
  data <- as.data.frame(data)
  # arguments <- as.list(match.call())
  # clustered <- !is.null(arguments$cluster)
  # if(clustered){
  #   createClusteredStanfile()
  # }else{
  #   createStanfile()
  # }
  clustered <- !is.null(cluster)
  if(clustered){
    createClusteredStanfile()
  }else{
    createStanfile()
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

  df1Rescaled <- as.data.frame(scale(df1))
  # put data in a list for stan
  if(clustered){
    data2 <- list(N = nrow(df1Rescaled),
                  K = K,
                  J = length(unique(df1[,cluster])),
                  y = df1Rescaled[,outcome],
                  x = matrix(df1Rescaled[,covariates]),
                  cluster = as.numeric(factor(df1Rescaled[,cluster])))
    # compile the model and run the sampler
    fit <- stan('clustered.stan', data=data2, chains = 4, iter = 2000, thin = 1, cores=min(parallel::detectCores(), 4), seed = 9782)

  }else{
    data2 <- list(N = nrow(df1Rescaled),
                  K = K,
                  y = df1Rescaled[,outcome],
                  x = matrix(df1Rescaled[,covariates]))
    # compile the model and run the sampler
    fit <- stan('unclustered.stan', data=data2, cores=min(parallel::detectCores(), 4), seed = 9782)
  }

  if(length(fit@model_pars) == 0) stop('Something went wrong and the fit object is empty :(')

  posteriorSamplesBeta <- t(t(extract(fit, pars='beta')[[1]]) * sdY/sdX)

  posteriorSamplesAlpha <- extract(fit, pars='alpha')[[1]] * sdY -
    rowSums(t(t(extract(fit, pars='beta')[[1]]) * sdY/sdX*meanX)) +
    meanY

  betas <- posteriorSamplesBeta %>% as.data.frame() %>%
    summarise_each(funs(mean, sd)) %>%
    tidyr::gather(variable, value) %>%
    tidyr::separate(variable, c("var", "stat"), sep = "\\_") %>%
    tidyr::spread(var, value) %>% select(-stat) %>%
    t() %>% as.data.frame()
  names(betas) <- c("coef", "SD")

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

  if(clustered){
    model <- createTexreg(
      coef.names = rownames(coef),
      coef = coef$coef,
      se = coef$SD,
      ci.low = coef$lb,
      ci.up = coef$ub,
      model.name = "Clustered Stan",
      gof.names = c("N", "Clusters "),
      gof = c(nrow(df1),
              length(unique(df1[,cluster]))),
      gof.decimal = c(FALSE, FALSE)
    )
  }else{
    model <- createTexreg(
      coef.names = rownames(coef),
      coef = coef$coef,
      se = coef$SD,
      ci.low = coef$lb,
      ci.up = coef$ub,
      model.name = "Unclustered Stan",
      gof.names = c("N"),
      gof = c(nrow(df1)),
      gof.decimal = c(FALSE)
    )
  }
  posteriorSamplesBeta <- as.data.frame(posteriorSamplesBeta)
  names(posteriorSamplesBeta) <- covariates
  output <- list(tbl = model,
                 posteriorSamples = list(posteriorSamplesBeta = posteriorSamplesBeta,
                                                      posteriorSamplesAlpha = posteriorSamplesAlpha),
                 fit = fit,
                 credible = credible)
  return(output)
}
