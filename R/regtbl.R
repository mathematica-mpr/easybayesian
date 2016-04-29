#' @title STAN linear model Regression Table
#' @export
#' @importFrom texreg screenreg htmlreg

regtbl <- function(model, type="text", star="\\*", ...){
  if(type=="text"){
    mynote <- paste0("* 0 outside the ", scales::percent(model$credible), " credible interval.\n",
                     "Rhat is the potential scale reduction factor on split chains (at convergence, Rhat=1).\n",
                     "$n_{eff}$ is a crude measure of effective sample size.\n",
                     "The log posterior quantifies the combined posterior density of all model parameters.")
    model.name <- paste0("Point Estimate \n",
                         "[", scales::percent(model$credible), " CI]")
    screenreg(model$tbl, custom.note = mynote,
              custom.columns = model$custom.columns,
              custom.model.names = model.name,
              custom.col.pos = c(2, 2), ...)
  }else{
    model.name <- paste0("Point Estimate<br>",
                         "[", scales::percent(model$credible), " CI]")
    mynote <- paste0(star, " 0 outside the ", scales::percent(model$credible), " credible interval.<br>",
                     "Rhat is the potential scale reduction factor on split chains (at convergence, Rhat=1).<br>",
                     "n_{eff} is a crude measure of effective sample size.<br>",
                     "The log posterior quantifies the combined posterior density of all model parameters.")
    htmlreg(model$tbl, star.symbol = star,
            custom.note = mynote,
            custom.columns = model$custom.columns,
            custom.model.names = model.name,
            ...)
  }
}
