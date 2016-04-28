library(shiny)
library(shinydashboard)
library(shinyjs)

# Header
header <-
  dashboardHeader(title = "Bayesian dashboard",
                  dropdownMenu(
                    type = "messages",
                    messageItem(from = "Ignacio",
                                message = "You can write something here."),
                    messageItem(
                      from = "New User",
                      message = "How do I register?",
                      icon = icon("question"),
                      time = "13:45"
                    ),
                    messageItem(
                      from = "Support",
                      message = "The new server is ready.",
                      icon = icon("life-ring"),
                      time = "2014-12-01"
                    )
                  ))

# Sidebar
sidebar <- dashboardSidebar(tags$head(
  tags$style(HTML("
                      .sidebar { height: 90vh; overflow-y: auto; }
                      " )
  )
),
  width = 250,
  fileInput(
    "chosenfile",
    label = h4("File input"),
    accept = ".csv"
  ),
  uiOutput("OutcomeVars"),
  uiOutput("TrtVars"),
  uiOutput("Controls"),
  uiOutput("ClusterVar"),
  numericInput(
    "cutoff",
    label = h4("Cutoff:"),
    value = 0,
    step = 0.01
  ),
  sliderInput(
    "credible",
    h4("Credible Interval Width:"),
    min = 50,
    max = 99,
    value = 75,
    step = 1,
    post = "%"
  ),
  column(3, actionButton(inputId = "go", label = "Run Analysis!"))
)

# Body

body <- dashboardBody(fluidPage(fluidRow(useShinyjs(),
  box(
    title = "Regression Table",
    status = "primary",
    solidHeader = TRUE,
    width = 6,
    uiOutput("regtable")
  ),

  box(
    title = "MCMC Traceplots",
    status = "warning",
    solidHeader = TRUE,
    width = 6,
    plotOutput("gof", height = "300px")
  )
)),
fluidRow(
  box(
    title = "Posterior Distribution of the Treatment Effect",
    status = "primary",
    solidHeader = TRUE,
    width = 6,
    checkboxInput("lessthan", "less than", value = FALSE, width = NULL),
    plotOutput("plot", click = "plot_click", height = "350px")
  ),

  box(
    title = "Interpretation",
    status = "primary",
    solidHeader = TRUE,
    width = 6,
    h4(uiOutput("interpretation"))
  )
))

shinyUI(dashboardPage(header, sidebar, body, skin = "red"))
