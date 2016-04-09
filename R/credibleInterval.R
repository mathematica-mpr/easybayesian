#' @title Credible Interval
#' @param posteriorSamples Posterior distribution samples
#' @param width CI width
#' @param digits digits for the CI
#' @return Credible interval
#' @export
credibleInterval <- function(posteriorSamples, width){
  ci <- quantile(posteriorSamples, probs = c((1 - width) / 2, 1 - ((1 - width) / 2)))
  return(ci)
}
