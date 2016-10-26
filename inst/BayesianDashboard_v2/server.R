# /******************************************************************************
# * Copyright (C) Mathematica Policy Research, Inc.
# * This code cannot be copied, distributed or used without the express written permission
# * of Mathematica Policy Research, Inc.
# *******************************************************************************/

library(shiny)
library(shinyjs)
library(shinyBS)
library(readr)
library(rstan)
library(easybayesian)
library(mongolite)
library(RJSONIO)
library(base64enc)
library(MPRDashboards)
library(sandwich)
library(lmtest)

db_live <- TRUE
db_connected <- TRUE

if (db_live) {
  db <- 'GateWayToRCTE'
  collections <- c('users', 'evaluations')

  db_connections <- list()

  for (collection in collections) {
    db_con <- try(mongo(
      url = 'mongodb://db1.edtechrce.org,db2.edtechrce.org',
      db = db, 
      collection = collection))

    if ('try-error' %in% class(db_con)) db_connected <- FALSE
    else db_connections[[collection]] <- db_con
  }
} else db_connected <- FALSE

shinyServer(function(input, output, session) {

  ids <- reactiveValues(
    user = NULL,
    evaluation = NULL)

  observe({
    # Wait for JS script to detect cookie, then parse and lookup
    connect.sid <- input$connect.sid

    #session$sendCustomMessage('log', sprintf('cookie: %s', connect.sid))

    if (db_live && db_connected && !is.null(connect.sid) && connect.sid != '') {
      session_query <- toJSON(list(userSession = connect.sid))

      user_match <- db_connections$users$find(
        session_query, fields = '{}')

      if ('data.frame' %in% class(user_match) && nrow(user_match) == 1) {
        ids$user <- user_match$`_id`
        ids$evaluation <- user_match$evaluation_id

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

  evaluation <- reactive({
    db_connections$evaluations$find(query = lookup_query())
  })

  # This will be read from the database in the production version
  db_values <- reactive({
    if (db_live && db_connected && identified()) {
      planQuestion <- evaluation()$planQuestion
      planNext <- evaluation()$planNext

      list(
        direction = tolower(planQuestion$Plan_Question_B_3),
        cutoff = planNext$Plan_Next_B,
        probability = as.numeric(gsub('%', '', planNext$Plan_Next_C_1, fixed=TRUE)))
    } else {
      list(
        direction = 'increased',
        cutoff = 1,
        probability = 75)
    }
  })

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

  diagnostic_error <- reactive({

    if (input$go > 0) {

      isolate({

        diagnostic_data <- data()

        if (is.null(diagnostic_data)) 'No data file has been uploaded, or the file was in an incorrect or corrupted format. Please check the file, or try refreshing the page and uploading again.'

        else if (nrow(diagnostic_data) == 0) 'The uploaded data file does not have any observations. Please check that the correct file was uploaded.'

        else if (is.null(input$outcome_var)) 'No outcome variable is selected. Please select the variable that indicates the outcome data.'

        else if (is.null(input$trt_var)) 'No treatment variable is selected. Please select the variable that indicates which observations used the app/training.'

        else if (!all(diagnostic_data[[input$trt_var]] %in% c(0, 1, NA))) 'Some values of the treatment variable are not 0 or 1 (or missing). Please check that the correct variable is selected, and that the data file contains the correct value for that variable.'

        else if (!all(input$control_vars %in% colnames(diagnostic_data))) 'One or more control variables do not exist in the data file. Check that you did not select the blank line at the top of the matching variable selector.'

        else if (any(sapply(diagnostic_data[, c(input$outcome_var, input$control_vars)], class) == 'character')) 'One or more of the outcome and control variables contains text values. Matching variables should only contain numeric values. Please check that the correct matching variables are selected and that the data file contains the correct values. One common issue is including text missing codes in the data. These should be changed to blank or ".".'

      })
    }
  })

  output$diagnostic_error <- renderPrint({
    HTML(sprintf('<div><p class="shiny-output-error">%s</p></div>', diagnostic_error()))
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

  results_by_grade <- reactive({

    if (input$go > 0) {

      if (is.null(diagnostic_error())) {

        disable('go')

        # Run regression
        if (length(input$control_vars) > 0) {
          mycontrols <- paste(input$control_vars, collapse = " + ")
          myformula <-
            paste0(input$outcome_var, " ~ ", input$trt_var, " + ", mycontrols)
        }
        else {
          myformula <- paste0(input$outcome_var, " ~ ", input$trt_var)
        }

        myformula <- as.formula(myformula)

        data_list <- data_by_grade()
        out <- list()

        n_grades <- length(data_list)
        multiple_grades <- n_grades > 1

        for (grade_i in seq_along(names(data_list))) {
          grade <- names(data_list)[grade_i]
          
          session$sendCustomMessage('update-progress-bar',
                                  round(100 * grade_i/ (n_grades + 1)))

          if (multiple_grades) detail <- sprintf('Analyzing grade %s', sanitize(grade))
          else detail <- 'Analyzing data'

          grade_data <- data_list[[grade]]

          if(input$cluster_var == "no cluster"){
            bayesian_lm1 <- try(stanlm(formula = myformula, data = grade_data))
          }else{
            bayesian_lm1 <- try(stanlm(formula = myformula, cluster = input$cluster_var, data = grade_data))
          }

          if (multiple_grades) title <- sprintf('Grade %s', grade)
          else title <- 'All grades combined'


          # Calculate frequentist model as well. Results will go in brief appendix.
          freq_lm1    <- try(lm(myformula, data = grade_data))

          if (!('try-error' %in% class(freq_lm1))) {
            freq_coef   <- coefficients(summary(freq_lm1))
            freq_impact <- freq_coef[input$trt_var, 'Estimate']

            if (input$cluster_var == 'no cluster') {
              freq_se     <- freq_coef[input$trt_var, 'Std. Error']
              freq_pvalue <- freq_coef[input$trt_var, 'Pr(>|t|)']
            }
            else {

              freq_cluster <- clustered.se(
                model_result = freq_lm1,
                data = grade_data,
                cluster = as.character(input$cluster_var),
                Tvar = as.character(input$trt_var),
                level = 0.95)

              freq_se     <- freq_cluster$standard.errors[input$trt_var]
              freq_pvalue <- freq_cluster$p.values[input$trt_var]
            }

            freq_lm1 <- list(
              outcome = input$outcome_var,
              impact= freq_impact,
              se    = freq_se,
              pvalue = freq_pvalue)
          }

          out[[grade]] <- list(
            bayesian = bayesian_lm1,
            freq = freq_lm1,
            grade = ifelse(multiple_grades, grade, title),
            title = title)
          
        }

        session$sendCustomMessage('update-progress-bar', 100)

        enable('go')

        out
      }
    }
  })

  results_updated <- reactive({
    if (input$go > 0) {

      lapply(
        results_by_grade(),
        FUN = function(result) {
          result$bayesian <- updateci(
            result$bayesian,
            credible = db_values()$probability / 100)

          result
        })
    }
  })

  # Updating by click is no longer supported when analysis is done by grade.
  # observeEvent(input$plot_click,{
  #   updateNumericInput(session, "cutoff",
  #                      value = round(input$plot_click$x,2))
  # })

  results_combined <- reactive({

    lapply(
      results_updated(),
      FUN = function(grade_output) {

        star <- "&#42"

        bayesian <- grade_output$bayesian

        mynote <- paste0(star, " 0 outside the ", scales::percent(bayesian$credible), " credible interval.<br>",
                   "The log posterior quantifies the combined posterior density of all model parameters.<br>",
                   "R&#770 is the potential scale reduction factor on split chains (at convergence, R&#770 = 1).<br>",
                   "N<sub>eff<//sub> is a crude measure of effective sample size."
                   )
        model.name <- paste0("Point Estimate<br>",
                       "[", scales::percent(bayesian$credible), " CI]")
        custom.columns <- list("R&#770"=bayesian$custom.columns$Rhat,
                         "N<sub>eff<//sub>"=bayesian$custom.columns$n_eff)

        trace <- bayesian$traceplots

        posterior <- posteriorplot(
          model = bayesian,
          parameter = input$trt_var , # input$trt_var, Treatment works
          cutoff = db_values()$cutoff, credibleIntervalWidth = db_values()$probability / 100,
          lessthan = (db_values()$direction == 'decreased'))

        interpretation <- interpret(model = bayesian,
                  name = input$trt_var,
                  cutoff = db_values()$cutoff,
                  credible = db_values()$probability / 100,
                  lessthan = (db_values()$direction == 'decreased'))

        interpretation_html <- HTML(paste0("<ul><li>",
                     interpretation$texts[[1]],
                     "</li><li>",
                     interpretation$texts[[2]],
                     "</li></ul>"))

        # Save posterior plot to tempfile, will be written to database and inserted into brief appendix as base64 encoded text.
        temp_plot <- tempfile()
        png(temp_plot)
          print(posterior)
        dev.off(which = dev.cur())

        list(
          output = list(
            HTML(sprintf('<h4>%s</h4>', sanitize(grade_output$title))),
            HTML('<h3>Regression Table</h3>'),
            HTML(
              texreg::htmlreg(bayesian$tbl, star.symbol = star,
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
            bayesian = list(
              interpretation = interpretation,
              posterior = base64encode(temp_plot)),
            freq = grade_output$freq,
            grade = grade_output$grade,
            title = grade_output$title))
      })
    })

    output$output_by_grade <- renderUI({
      list(
        lapply(results_combined(),
          FUN = `[[`,
          'output'),
        
        HTML('<p><strong>To learn more</strong> about how this dashboard works, you can review this <a target="_new" href="https://edtechrce.org/static/pdf/05.02a%20Impact%20Estimation%20Technical%20Appendix.pdf">technical appendix.</a></p>'))
    })

    # Save required results to database
    observe({
      if (!is.null(results_combined()) && length(results_combined()) > 0) {

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
                  impacts = list(
                    results = db_results_json))))
  
            success <- db_connections$evaluations$update(
              query = lookup_query(),
              update = update,
              upsert = TRUE)
  
            if (success) {
              output$save_status <- renderPrint(h5('Results saved successfully!'))
            }
            else {
               output$save_status <- renderPrint(HTML('<div><p class="shiny-output-error">Your results were not saved due to a database error. Please try again. If the same error occurs, contact the administrator.</p></div>'))
            }
          }
          else {
            output$save_status <- renderPrint(HTML('<div><p style="color: red;"><strong>Warning:</strong> You are not currently logged in. Results will not be saved for future use. To save results as part of a full evaluation, please log in before beginning the impact calculation exercise.</p></div>'))
  
          }
        })
      }
    })

})
