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

db_live <- TRUE
db_connected <- TRUE

if (db_live) {
  db <- 'GateWayToRCTE'
  collections <- c('users', 'plannexts', 'planquestions', 'impacts')

  db_connections <- list()

  for (collection in collections) {
    db_con <- try(mongo(db = db, collection = collection))

    if ('try-error' %in% class(db_con)) db_connected <- FALSE
    else db_connections[[collection]] <- db_con
  }
} else db_connected <- FALSE

shinyServer(function(input, output, session) {

  ids <- reactiveValues(
    user_id = NULL,
    evaluation_id = NULL)

  observe({
    # Wait for JS script to detect cookie, then parse and lookup
    #cookie <- input$cookie
    cookie <- 'connect.sid=s%3AMTMOmAOTdtyF0sPW5OOJZkdtiD_GTZZw.FUj7Z7v5Dc2%2BC2r4ia88Th5RcLlEZzawJzHhVFPeohc'

    if (db_live && db_connected) {
      session_query <- toJSON(list(userSession = cookie))

      user_match <- db_connections$users$find(
        session_query, fields = '{}')

      if ('data.frame' %in% class(user_match) && nrow(user_match) == 1) {
        ids$user <- sanitize(user_match$`_id`)
        ids$evaluation <- sanitize(user_match$evaluation_id)

        isolate({
          if (is.null(ids$evaluation)) ids$evaluation <- ids$user
        })
      }
    }
  })

  identified <- reactive({
    !is.null(ids$user)
  })

  lookup_query <- reactive({
    toJSON(list(
      userid = list(`$oid` = ids$user)))
      #evaluationid = list(`$oid` = ids$evaluation)))
  })

  # This will be read from the database in the production version
  db_values <- reactive({
    if (db_live && db_connected && identified()) {
      planquestions <- db_connections$planquestions$find(query = lookup_query())
      plannexts <- db_connections$plannexts$find(query = lookup_query())

      list(
        direction = tolower(sanitize(planquestions$Plan_Question_B_3)),
        cutoff = sanitize(plannexts$Plan_Next_B),
        probability = sanitize(plannexts$Plan_Next_C_1))
    } else {
      list(
        direction = 'increased',
        cutoff = 1,
        probability = 75)
    }
  })



  results <- reactiveValues()
  # Load the chosen dataset
  data <- reactive({
    dfile <- input$chosenfile[1, 4]  #<-filename with path is the [1,4] cell in obj
    if (!is.null(dfile)) {
      read_csv(dfile)
    }
  })

  observe({
    if (!is.null(data())) {
      session$sendCustomMessage('confirm_upload', 'Upload complete')
    }
  })

  #Outcome Var
  observe({

    updateSelectizeInput(
      session = session,
      inputId = 'outcome_var',
      choices = colnames(data()))
  })

  #Treatment Var
  observe({

    updateSelectizeInput(
      session = session,
      inputId = 'trt_var',
      choices = setdiff(colnames(data()), input$outcome_var))
  })

  #Control Vars
  observe({

    updateSelectizeInput(
      session = session,
      inputId = 'control_vars',
      choices = setdiff(colnames(data()), c(input$outcome_var, input$trt_var)))

  })

  #Clustering Var
  observe({

    updateSelectizeInput(
      session = session,
      inputId = 'cluster_var',
      choices = c("no cluster", setdiff(colnames(data()), c(input$outcome_var, input$trt_var, input$control_vars))))

  })

  # Grade variable
  observe({

    updateSelectizeInput(
      session = session,
      inputId = 'grade_var',
      choices = c('combine all grades', setdiff(colnames(data()), c(input$outcome_var, input$trt_var, input$control_vars, input$cluster_var))))

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
    if (!is.null(input$grade_var) && input$grade_var %in% colnames(data())) {
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

        if (multiple_grades) detail <- sprintf('Analyzing grade %s', sanitize(grade))
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
        else lm1$title <- 'All grades combined'

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
      credible = db_values()$probability / 100)
  })

  # Updating by click is no longer supported when analysis is done by grade.
  # observeEvent(input$plot_click,{
  #   updateNumericInput(session, "cutoff",
  #                      value = round(input$plot_click$x,2))
  # })

  results_combined <- reactive({

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
          cutoff = db_values()$cutoff, credibleIntervalWidth = db_values()$probability / 100,
          lessthan = (db_values()$direction == 'decreased'))

        interpretation <- interpret(model = grade_output,
                  name = input$trt_var,
                  cutoff = db_values()$cutoff,
                  credible = db_values()$probability / 100,
                  lessthan = (db_values()$direction == 'decreased'))

        interpretation_html <- HTML(paste0("<ul><li>",
                     interpretation$texts[[1]],
                     "</li><li>",
                     interpretation$texts[[2]],
                     "</li></ul>"))


        list(
          output = list(
            HTML(sprintf('<h4>%s</h4>', sanitize(grade_output$title))),
            HTML('<h3>Regression Table</h3>'),
            HTML(
              texreg::htmlreg(grade_output$tbl, star.symbol = star,
                              custom.note = mynote,
                              custom.columns = custom.columns,
                              caption = "",
                              custom.model.names = model.name)
            ),
            HTML('<h3>MCMC Trace Plots</h3>'),
            renderPlot(trace),
            HTML('<h3>Posterior distribution of treatment effect</h3>'),
            renderPlot(posterior),
            HTML('<h3>Interpretation</h3>'),
            h4(interpretation_html)
          ),
          db_input = list(
            title = sanitize(grade_output$title),
            interpretation = sanitize(interpretation)))
      })
    })

    output$output_by_grade <- renderUI({
      lapply(results_combined(),
        FUN = `[[`,
        'output')
    })

    # Save required results to database
    observe({
      results_combined()

      isolate({
        if (db_live && db_connected && identified()) {

          db_results <- lapply(
            results_combined(),
            FUN = `[[`,
            'db_input')

          db_results_json <- toJSON(rawToChar(serialize(db_results, connection = NULL, ascii = TRUE)))

          update <- toJSON(
            list(
              `$set` = list(
                results = db_results_json)))

          success <- db_connections$impacts$update(
            query = lookup_query(),
            update = update,
            upsert = TRUE)

           if (!success) {
             output$save_status <- renderPrint(HTML('<div><p class="shiny-output-error">Your results were not saved due to a database error. Please try saving again. If the same error occurs, contact the administrator</p></div>'))
           }
        }
        else {
          output$save_status <- renderPrint(HTML('<div><p style="color: red;"><strong>Warning:</strong> You are not currently logged in. The matching tool successfully produced results, including a downloadable file, but these results will not be saved for future use. To save results as part of a full evaluation, please log in and repeat the matching exercise.</p></div>'))

        }
      })
    })

    observe({
      shinyjs::toggleState("go",!is.null(input$trt_var) && input$trt_var != "")
    })

})
