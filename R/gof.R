#' @title Goodness of fit
#' @export
#' @import rstan
#' @importFrom pander pandoc.table
#' @import htmlTable

gof.table <- function(model, caption = "", type="grid", digits=2) {
  K <- length(model$tbl@coef.names)
  gof <- data.frame(Rhat = summary(model$fit)$summary[,"Rhat"][c(2:K,1)],
             n_eff = summary(model$fit)$summary[,"n_eff"][c(2:K,1)])
  rownames(gof) <- model$tbl@coef.names

  if(type=="html"){
    htmlTable(txtRound(gof, digits = digits))
  }else{
    pandoc.table(gof, style = "grid", caption = caption)
  }
}
