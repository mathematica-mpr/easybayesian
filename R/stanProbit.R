#' @title STAN Probit model
#' @export
#' 
stanprobit <- function(formula, cluster=NULL, data, credible = .95,
                       chains = 4, iter = 2000, thin = 1){
  #browser()
  data <- as.data.frame(data)
  clustered <- !is.null(cluster)
  if(clustered){
    stop("this is not implemented yet")
  }else{
    createStanprobitfile()
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
  df1Rescaled[,outcome] <- df1[,outcome]
  # put data in a list for stan
  if(clustered){
    data2 <- list(N = nrow(df1Rescaled),
                  K = K,
                  J = length(unique(df1[,cluster])),
                  y = df1Rescaled[,outcome],
                  x = as.matrix(df1Rescaled[,covariates]),
                  cluster = as.numeric(factor(df1Rescaled[,cluster])))
    # compile the model and run the sampler
    fit <- stan('probitclustered.stan', data=data2, 
                chains = chains, iter = iter, thin = thin, 
                cores=min(parallel::detectCores(), 4), seed = 9782)
    
  }else{
    data2 <- list(N = nrow(df1Rescaled),
                  K = K,
                  y = df1Rescaled[,outcome],
                  x = as.matrix(df1Rescaled[,covariates]))
    # compile the model and run the sampler
    fit <- stan('probit.stan', data=data2,
                chains = chains, iter = iter, thin = thin,
                cores=min(parallel::detectCores(), 4), seed = 9782)
  }
  
  if(length(fit@model_pars) == 0) stop('Something went wrong and the fit object is empty :(')
  
  posteriorSamplesBeta <- t(t(extract(fit, pars='beta')[[1]]) * sdY/sdX)
  
  posteriorSamplesAlpha <- extract(fit, pars='alpha')[[1]] * sdY -
    rowSums(t(t(extract(fit, pars='beta')[[1]]) * sdY/sdX*meanX)) +
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
  traceplots <- traceplot(fit, pars = c(names(fit)[1:(K+1)], "lp__"))
  
  output <- list(tbl = model,
                 posteriorSamples = list(posteriorSamplesBeta = posteriorSamplesBeta,
                                         posteriorSamplesAlpha = posteriorSamplesAlpha),
                 fit = fit,
                 credible = credible,
                 custom.columns = custom.columns,
                 traceplots = traceplots)
  return(output)
}