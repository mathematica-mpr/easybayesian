#' @title Update Credible Interval
#' @export
#' @importClassesFrom rstan stanfit
updateci <- function(model, conf){
  fit <-  model$fit
  K <- length(lm1$tbl@coef.names) - 1
  posteriorSamplesBeta <-model$posteriorSamples$posteriorSamplesBeta
  cibetas <- data.frame(t(sapply(1:K, function(j)credibleInterval(posteriorSamplesBeta[,j], conf))))
  names(cibetas) <- c("lb", "ub")
  posteriorSamplesAlpha <- model$posteriorSamples$posteriorSamplesAlpha
  cialpha <- credibleInterval(posteriorSamplesAlpha, conf)
  cialpha <- data.frame(lb = cialpha[[1]], ub = cialpha[[2]])
  ci <- bind_rows(cibetas, cialpha)
  rownames(ci) <- model$tbl@coef.names

  if(model$tbl@model.name == "Clustered Stan"){
    model <- createTexreg(
      coef.names = model$tbl@coef.names,
      coef = model$tbl@coef,
      se = model$tbl@se,
      ci.low = ci$lb,
      ci.up = ci$ub,
      model.name = "Clustered Stan",
      gof.names = model$tbl@gof.names,
      gof = model$tbl@gof,
      gof.decimal = model$tbl@gof.decimal
    )

  }else
  {
    model <- createTexreg(
      coef.names = model$tbl@coef.names,
      coef = model$tbl@coef,
      se = model$tbl@se,
      ci.low = ci$lb,
      ci.up = ci$ub,
      model.name = "Unclustered Stan",
      gof.names = model$tbl@gof.names,
      gof = model$tbl@gof,
      gof.decimal = model$tbl@gof.decimal
    )
  }
  output <- list(tbl = model,
                 posteriorSamples = list(posteriorSamplesBeta = posteriorSamplesBeta,
                                         posteriorSamplesAlpha = posteriorSamplesAlpha),
                 fit = fit)
  return(output)

}
