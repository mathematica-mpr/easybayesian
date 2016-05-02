#' Bayesian dashboard
#'
#' @export
#' @import shiny
#' @import shinydashboard


bayesiandashboard <- function() {
  shiny::runApp(system.file('BayesianDashboard', package='easybayesian'))
}
