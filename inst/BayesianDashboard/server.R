library(shiny)
library(shinyjs)
library(rstan)
library(easybayesian)
library(readr)

shinyServer(function(input, output, session) {
  results <- reactiveValues()
  #Load the chosen dataset
  data <- reactive({
    dfile <-
      input$chosenfile[1, 4] #<-filename with path is the [1,4] cell in obj
    if (!is.null(dfile))
      read_csv(dfile)
  })

  #Outcome Var
  output$OutcomeVars <- renderUI({
    selectizeInput(
      "outcome_var",
      label = h4("Outcome variable"),
      choices = colnames(data()),
      multiple = FALSE,
      selected = NULL,
      options = list(placeholder = "Select the outcome variable")
    )
  })

  #Treatment Var
  output$TrtVars <- renderUI({
    selectizeInput(
      "trt_var",
      label = h4("Treatment variable"),
      choices = setdiff(colnames(data()), input$outcome_var),
      multiple = FALSE,
      selected = NULL,
      options = list(placeholder = "Select the treatment variable")
    )
  })

  #Control Vars
  output$Controls <- renderUI({
    selectizeInput(
      "control_vars",
      label = h4("Control variables"),
      choices = setdiff(colnames(data()), c(input$trt_var, input$outcome_var)),
      multiple = TRUE,
      selected = NULL,
      options = list(placeholder = "Select control variables")
    )
  })

  #Clustering Var
  output$ClusterVar <- renderUI({
    selectizeInput(
      "cluster_var", label = h4("Cluster variable"),
      choices = c("no cluster", setdiff(colnames(data()), c(input$outcome_var, input$trt_var, input$control_vars))),
      multiple = FALSE,
      selected = NULL,
      options = list(placeholder = "Select the cluster variable")
    )
  })

  observeEvent(input$go, {
    # Run regression
    if (length(input$control_vars) > 0) {
      mycontrols <- paste(input$control_vars, collapse = " + ")
      myformula <-
        paste0(input$outcome_var, " ~ ", input$trt_var, " + ", mycontrols)
    }
    else {
      myformula <- paste0(input$outcome_var, " ~ ", input$trt_var)
    }

    withProgress(message = 'Runnig Baysian Model', detail = "reading data", value = 0, {
      df1 <- as.data.frame(data())
      incProgress(0.1, detail = "be patient")
      if(input$cluster_var == "no cluster"){
        lm1 <- stanlm(formula = as.formula(myformula), data = df1)
      }else{
        lm1 <- stanlm(formula = as.formula(myformula), cluster = input$cluster_var, data = df1)
      }


      incProgress(0.75, detail = "almost there")
      results[[as.character(length(names(results)) + 1)]] <- lm1
      setProgress(1)
    })

  }) #<-end observeEvent

  lmupdated <- reactive({
    if (input$go == 0)
      return()
    else
    lista <- reactiveValuesToList(results)
    lm <- lista[[length(lista)]]
    updatedlm <- updateci(lm, credible = input$credible / 100)
  })

  output$regtable <- renderUI({
    if (input$go == 0)
      return()
    else
      lm <- lmupdated()
      HTML(
        regtbl(lm, type="html", star = "&#42")
      )
  })

    output$plot <- renderPlot({
      if (input$go == 0)
        return()
      else
      lista <- reactiveValuesToList(results)
      lm <- lista[[length(lista)]]
      #parameter <- as.character(input$trt_var)
      posteriorplot(model = lm, parameter = input$trt_var , # input$trt_var, Treatment works
                    cutoff = input$cutoff, credibleIntervalWidth = input$credible / 100)

    })
    
    observeEvent(input$plot_click,{
      updateNumericInput(session, "cutoff", 
                         value = round(input$plot_click$x,2))
    })

    output$interpretation <- renderUI({
      if (input$go == 0)
        return()
      else
        lista <- reactiveValuesToList(results)
      lm <- lista[[length(lista)]]
      interpretation <-
        interpret(model = lm,
                  name = input$trt_var,
                  cutoff = input$cutoff,
                  credible = input$credible / 100)
      text <- paste0("<ul><li>",
                     interpretation[[1]],
                     "</li><li>",
                     interpretation[[2]],
                     "</li></ul>")
      text <-  text[[length(text)]]
      HTML(text)
    })

    output$gof <- renderUI({
      if (input$go == 0)
        return()
      else
        lista <- reactiveValuesToList(results)
      lm <- lista[[length(lista)]]
      HTML(gof.table(lm, type="html"))
    })

    observe({
      shinyjs::toggleState("go",!is.null(input$trt_var) && input$trt_var != "")
    })

})
