#' @title STAN linear model Regression Table
#' @export
#' @importFrom texreg screenreg htmlreg

regtbl <- function(model, type="text", ...){
  if(type=="text"){
    screenreg(model$tbl, custom.note = "* 0 outside the credible interval.", ...)
  }else{
    htmlreg(model$tbl, star.symbol = "\\*",  custom.note = "\\* 0 outside the credible interval.", ...)
  }
}
