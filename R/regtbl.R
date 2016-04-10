#' @title STAN linear model Regression Table
#' @export
#' @importFrom texreg screenreg htmlreg

regtbl <- function(model, type="text", star="\\*", ...){
  if(type=="text"){
    screenreg(model$tbl, custom.note = "* outside the credible interval.", ...)
  }else{
    htmlreg(model$tbl, star.symbol = star,
            custom.note = paste0(star," 0 outside the credible interval."), ...)
  }
}
