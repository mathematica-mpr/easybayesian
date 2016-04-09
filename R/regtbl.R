#' @title STAN linear model Regression Table
#' @export
#' @importFrom texreg screenreg htmlreg

regtbl <- function(model, type="text", ...){
  if(type=="text"){
    screenreg(model$tbl, ...)
  }else{
    htmlreg(model$tbl, ...)
  }
}
