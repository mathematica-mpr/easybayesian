#' @title Interpret lm stan
#' @export
#' @import scales

interpret <- function(model, name, cutoff){
  arguments <- as.list(match.call())
  whichParameter <- which(model$tbl@coef.names==arguments$name)
  point <- model$tbl@coef[[whichParameter]]
  lb <- round(model$tbl@ci.low[[whichParameter]],2)
  ub <-  round(model$tbl@ci.up[[whichParameter]],2)
  posteriorSamples <- model$posteriorSamples$posteriorSamplesBeta
  posteriorProbability <- apply(posteriorSamples, 2, function(x) return(mean(x>cutoff)))
  prob <- scales::percent(posteriorProbability[whichParameter])
  credible <- scales::percent(model$credible)
  text1 <- paste0(
    "There is a ",
    credible,
    " probability that the true impact of the intervention is between ",
    lb,
    " and ",
    ub,
    " units."
  )

  text2 <- paste0(
    "There is a ",
    prob,
    " probability that the intervention increases the outcome by ",
    cutoff,
    " units or more."
  )
  texts <- list(text1, text2)

  return(texts)
}
