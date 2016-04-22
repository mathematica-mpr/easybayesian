#' @title STAN linear model Regression Table
#' @export
#' @importFrom texreg screenreg htmlreg

regtbl <- function(model, type="text", star="\\*", ...){
  if(type=="text"){
    mynote <- paste0("* outside the ", scales::percent(model$credible), " credible interval")
    screenreg(model$tbl, custom.note = mynote,
              custom.columns = model$custom.columns,
              custom.col.pos = c(2, 2), ...)
  }else{
    mynote <- paste0(star, " outside the ", scales::percent(model$credible), " credible interval")
    htmlreg(model$tbl, star.symbol = star,
            custom.columns = model$custom.columns,
            custom.col.pos = c(2, 2),
            custom.note = mynote, ...)
  }
}
