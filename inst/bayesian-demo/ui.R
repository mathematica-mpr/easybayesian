# /******************************************************************************
# * Copyright (C) Mathematica Policy Research, Inc.
# * This code cannot be copied, distributed or used without the express written permission
# * of Mathematica Policy Research, Inc.
# *******************************************************************************/
library(shiny)
library(MPRDashboards)

htmlTemplate("template.html",
   header = html_header(),
   title = html_title(
     title = 'Get Results: Find Out If the App Moved the Needle',
     subtitle = '<p><strong>Objective:</strong> In this step, you will calculate the effect of the app. By uploading a data file and answering a few key questions, you will arrive at a rigorous answer to the research question you crafted at the beginning of this wizard.</p>'
   ),
   # intro = html_step(
   #   header = 'Overview: Bayesian analysis',
   #   info = '<p><b>Challenge:</b> You want to do some Bayesian analysis.</p><p><b>Solution:</b> Use this tool!</p>',
   #   action = NULL
   # ),
   upload_data = html_step(
     header = 'STEP 1',
     subheader = 'Upload your data',
     info = '<p>Upload a <a href="#">CSV</a> file of your data. The file should have one row for each person in your sample that you collected outcome data (e.g. test scores) on, and one column for each field (for example, test score and background characteristics). Read <a href="#">this guide</a> for details.</p>',
     action = html_action(
       uiOutput('upload_button'),
       id = 'upload')
   ),
   outcome_var = html_step(
     header = 'STEP 2',
     subheader = 'What is the outcome variable?',
     info = '<p>Choose the name of the variable that measures the outcome you are studying.</p>',
     action = html_action(
       uiOutput('OutcomeVars')
     )
   ),
   treatment_var = html_step(
     header = 'STEP 3',
     subheader = 'Who is getting the app/training?',
     info = '<p>Choose the name of the variable that tells us who is using the app/training. Remember, this variable should equal 1 for users and 0 for non-users.</p>',
     action = html_action(
       uiOutput('TrtVars')
     )
   ),
   control_vars = html_step(
     header = 'STEP 4',
     subheader = 'Control variables',
     info = '<p>What variables, if any, should we control for while estimating impacts?</p>',
     action = html_action(
      uiOutput('Controls')
     )
   ),
   # cluster_var = html_step(
   #   header = 'STEP 5',
   #   subheader = 'Cluster variable',
   #   info = "<p>If users and non-users were chosen based on groups they belonged to rather than as individuals, we need to identify these \"<em>clusters</em>\" in order to correctly estimate the effect of the intervention. If the decision to give students an app was made by principals, and all students in a school either get the app or don't, then select the \"School ID\" variable or some other indicator of which school the student attended. Similarly, if the intervention is at the classroom level, the clusters are the classrooms.  What variable, if any, should we use as a cluster indicator?</p>",
   #   action = html_action(
   #    uiOutput('ClusterVar')
   #   )
   # ),
   outcome_change = html_step(
     header = 'STEP 6',
     subheader = 'Minimum change',
     info = uiOutput('Q_BD_1')
   ),
   # grade_var = html_step(
   #   header = 'STEP 7',
   #   subheader = 'Should analysis be conducted by grade?',
   #   info = '<p>In some cases it is important that analysis be conducted within groups of students in the same grade. If this does not matter for your evaluation, leave this as "combine all grades" in the select menu. If you want to analyze students separtely by grade, please select the name of the variable that indicates student grades.</p>',
   #   action = html_action(
   #     uiOutput('grade_var')
   #   )
   # ),
   cutoff = html_step(
     header = 'STEP 8',
     subheader = 'Cutoff',
     info = '<p>Select a cutoff value.</p>',
     action = html_action(
      numericInput(
        inputId = 'cutoff',
        label = '',
        value = 0,
        step = 0.01
      )
     )
   ),
   decisions = html_step(
     header = 'STEP 9',
     subheader = 'Plan decisions based on your findings',
     info = uiOutput('Q_BD_2ab')
   ),
   credible_interval = html_step(
     header = NULL,
     subheader = 'Credible interval',
     info = uiOutput('Q_BD_3')
   ),
   analyze_button = actionButton(inputId = "go", label = "Run Analysis!"),
   output_by_grade = uiOutput('output_by_grade'),
   
   posterior_plot_header = html_step(
     header = "Posterior Distribution of the Treatment Effect"
   ),
   posterior_plot = plotOutput("posterior_plot", click = "plot_click", height = "350px")
)

