#' @title STAN linear model Regression Table
#' @export
#' @importFrom texreg screenreg htmlreg

regtbl <- function(model, type="text", ...){
  if(type=="text"){
    screenreg(model$tbl, custom.note = "* outside the credible interval.", ...)
  }else{
    htmlreg(model$tbl, star.symbol = "&#42",  custom.note = "&#42 0 outside the credible interval.", ...)
  }
}
