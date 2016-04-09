#' @title Goodness of fit
#' @export
#' @import rstan

gof.table <- function(model) {
  K <- length(model$tbl@coef.names)
  gof <- data.frame(Rhat = summary(model$fit)$summary[,"Rhat"][c(2:K,1)],
             n_eff = summary(model$fit)$summary[,"n_eff"][c(2:K,1)])
  rownames(gof) <- model$tbl@coef.names
  return(gof)
}
