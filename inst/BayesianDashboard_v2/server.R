# /******************************************************************************
# * Copyright (C) Mathematica Policy Research, Inc.
# * This code cannot be copied, distributed or used without the express written permission
# * of Mathematica Policy Research, Inc.
# *******************************************************************************/

library(shiny)
library(shinyBS)
library(readr)
library(rstan)
library(easybayesian)
library(mongolite)
library(RJSONIO)
library(base64enc)

shinyServer(function(input, output, session) {
  
  # This will be read from the database in the production version
  db <- list(
    lessthan = FALSE,
    Q_BD_1 = 1)
  
  results <- reactiveValues()
  # Load the chosen dataset
  data <- reactive({
    dfile <- input$chosenfile[1, 4]  #<-filename with path is the [1,4] cell in obj
    if (!is.null(dfile)) {
      read_csv(dfile)
    }
  })

  output$upload_button <- renderUI({
    if (is.null(data())) {
      placeholder <- 'Upload your data'
      button_text <- 'Choose File'
    } else {
      placeholder <- 'Upload complete'
      button_text <- ''
    }

    html_template_file_input(
      placeholder = placeholder,
      button_text = button_text,
      accept      = '.csv')
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
  
  # Grade variable
  output$grade_var <- renderUI({
    selectizeInput(inputId = "grade_var",
      label = label_with_help(label = "Grade indicator", id = "grade_var_modal", size=h4),
      choices = c('combine all grades', setdiff(colnames(data()), input$trt_var)),
      multiple = TRUE,
      selected = 'combine all grades',
      options = list(placeholder = "Select grade variable", maxItems = 1))
  })

  #Control Vars
  output$Controls <- renderUI({
    selectizeInput(
      "control_vars",
      label = h4("Control variables"),
      choices = setdiff(colnames(data()), c(input$trt_var, input$outcome_var, input$grade_var)),
      multiple = TRUE,
      selected = NULL,
      options = list(placeholder = "Select control variables")
    )
  })

  #Clustering Var
  output$ClusterVar <- renderUI({
    selectizeInput(
      "cluster_var", label = h4("Cluster variable"),
      choices = c("no cluster", setdiff(colnames(data()), c(input$outcome_var, input$trt_var, input$grade_var, input$control_vars))),
      multiple = FALSE,
      selected = NULL,
      options = list(placeholder = "Select the cluster variable")
    )
  })
  
  output$Q_BD_3 <- renderUI({
    div(
      HTML(sprintf("<p>We can also identify a range that the app's effect falls within, with a level of confidence you choose below. We can tell you that there is a 75%% probability that the true impact of the intervention is between %s and %s units. As you adjust the probability below, the range changes. There is a ",
      '[F.BD.4]',
      '[F.BD.5]')),
      tags$input(
        id = "Q_BD_3",
        type = "number", 
        value = 75,
        min = 1, 
        max = 100,
        style = "width:40px"),
      HTML(sprintf('%% probability that the true impact of the intervention is between %s and %s units.',
      '[F.BD.4]',
      '[F.BD.5]')))
    
  })
  
  data_by_grade <- reactive({
    if (input$grade_var %in% colnames(data())) {
      index <- data()[, input$grade_var]
    }
    else {
      index <- rep('constant', nrow(data()))
    }
    
    by(
      data = data(),
      INDICES = index,
      FUN = function(x) x)
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
    
    data_list <- data_by_grade()
    results_by_grade <- list()
    
    n_grades <- length(data_list)
    multiple_grades <- n_grades > 1
    
    withProgress(message = 'Running Bayesian Model', detail = "reading data", value = 0, {
      
      for (grade_i in seq_along(names(data_list))) {
        grade <- names(data_list)[grade_i]
        
        if (multiple_grades) detail <- sprintf('Analyzing grade %s', grade)
        else detail <- 'Analyzing data'
        
        setProgress(
          message = 'Running Bayesian Model',
          detail = detail,
          value = (max(0.5, grade_i - 1) / n_grades))

        grade_data <- data_list[[grade]]
        
        if(input$cluster_var == "no cluster"){
          lm1 <- try(stanlm(formula = as.formula(myformula), data = grade_data))
        }else{
          lm1 <- try(stanlm(formula = as.formula(myformula), cluster = input$cluster_var, data = grade_data))
        }
        
        if (multiple_grades) lm1$title <- sprintf('Grade %s', grade)
        else lm1$title <- ''
        
        results_by_grade[[grade]] <- lm1
      }
      
      results[[as.character(length(names(results)) + 1)]] <- results_by_grade
      
      setProgress(1)
    })

  }) #<-end observeEvent

  lmupdated <- reactive({
    if (input$go == 0)
      return()
    else
    lista <- reactiveValuesToList(results)
    results_by_grade <- lista[[length(lista)]]

    lapply(
      results_by_grade,
      updateci,
      credible = input$Q_BD_3 / 100)
  })

  # Updating by click is no longer supported when analysis is done by grade.
  # observeEvent(input$plot_click,{
  #   updateNumericInput(session, "cutoff",
  #                      value = round(input$plot_click$x,2))
  # })

  output$output_by_grade <- renderUI({
    
    lapply(
      lmupdated(),
      FUN = function(grade_output) {
        
        star <- "&#42"
        #lm <- lmupdated()
        mynote <- paste0(star, " 0 outside the ", scales::percent(grade_output$credible), " credible interval.<br>",
                   "The log posterior quantifies the combined posterior density of all model parameters.<br>",
                   "R&#770 is the potential scale reduction factor on split chains (at convergence, R&#770 = 1).<br>",
                   "N<sub>eff<//sub> is a crude measure of effective sample size."
                   )
        model.name <- paste0("Point Estimate<br>",
                       "[", scales::percent(grade_output$credible), " CI]")
        custom.columns <- list("R&#770"=grade_output$custom.columns$Rhat,
                         "N<sub>eff<//sub>"=grade_output$custom.columns$n_eff)
        
        trace <- grade_output$traceplots
        
        posterior <- posteriorplot(
          model = grade_output,
          parameter = input$trt_var , # input$trt_var, Treatment works
          cutoff = db$Q_BD_1, credibleIntervalWidth = input$Q_BD_3 / 100,
          lessthan = db$lessthan)
        
        interpretation <- interpret(model = grade_output,
                  name = input$trt_var,
                  cutoff = db$Q_BD_1,
                  credible = input$Q_BD_3 / 100,
                  lessthan = db$lessthan)
        
        interpretation <- HTML(paste0("<ul><li>",
                     interpretation[[1]],
                     "</li><li>",
                     interpretation[[2]],
                     "</li></ul>"))
        
        list(
          h4(grade_output$title),
          html_step(
            header = 'Regression Table'
          ),
          HTML(
            texreg::htmlreg(grade_output$tbl, star.symbol = star,
                            custom.note = mynote,
                            custom.columns = custom.columns,
                            caption = "",
                            custom.model.names = model.name)
          ),
          html_step(
            header = 'MCMC trace plots'
          ),
          renderPlot(trace),
          html_step(
            header = 'Posterior distribution of treatment effect'
          ),
          renderPlot(posterior),
          html_step(
            header = 'Interpretation'
          ),
          h4(interpretation)
        ) 
      })
    })

    observe({
      shinyjs::toggleState("go",!is.null(input$trt_var) && input$trt_var != "")
    })

})
