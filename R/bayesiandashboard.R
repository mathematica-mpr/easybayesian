#' Bayesian dashboard
#'
#' @export
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import readr
#' @import rstan


bayesiandashboard <- function() {
  shiny::runApp(system.file('BayesianDashboard', package='easybayesian'))
}
