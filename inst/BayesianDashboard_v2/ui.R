# /******************************************************************************
# * Copyright (C) Mathematica Policy Research, Inc.
# * This code cannot be copied, distributed or used without the express written permission
# * of Mathematica Policy Research, Inc.
# *******************************************************************************/
library(shiny)
library(shinyjs)
library(MPRDashboards)

htmlTemplate("template.html",
   header = html_header(),
   title = html_tool.title(
     title = 'Get Results: Find Out If the App Moved the Needle',
     subtitle = '<p><strong>Objective:</strong> In this step, you will calculate the effect of the app. By uploading a data file and answering a few key questions, you will arrive at a rigorous answer to the research question you crafted at the beginning of this wizard.</p>'
   ),
   # intro = html_step(
   #   header = 'Overview: Bayesian analysis',
   #   info = '<p><b>Challenge:</b> You want to do some Bayesian analysis.</p><p><b>Solution:</b> Use this tool!</p>',
   #   action = NULL
   # ),
   upload_data = html_step(
     header = 'Upload your data',
     subheader = 'Upload a <a href="#">CSV</a> file of your data. The file should have one row for each person in your sample that you collected outcome data (e.g. test scores) on, and one column for each field (for example, test score and background characteristics). Read <a href="#">this guide</a> for details.',
     data.target = "A",
     action = html_action(
       uiOutput('upload_button'),
       id = 'upload')
   ),
   outcome_var = html_step(
     header = 'What is the outcome variable?',
     subheader = '<p>Choose the name of the variable that measures the outcome you are studying.</p>',
     data.target = "B",   
     action = html_action(
       uiOutput('OutcomeVars')
     )
   ),
   treatment_var = html_step(
     header = 'Who is getting the app/training?',
     subheader = 'Choose the name of the variable that tells us who is using the app/training. Remember, this variable should equal 1 for users and 0 for non-users.',
     data.target = "C", 
     action = html_action(
       uiOutput('TrtVars')
     )
   ),
   control_vars = html_step(
     header = 'Control variables',
     subheader = '<p>What variables, if any, should we control for while estimating impacts?</p>',
     data.target = "D",    
     action = html_action(
      uiOutput('Controls')
     )
   ),
   cluster_var = html_step(
     header = 'Cluster variable',
     subheader = "If users and non-users were chosen based on groups they belonged to rather than as individuals, we need to identify these \"<em>clusters</em>\" in order to correctly estimate the effect of the intervention. If the decision to give students an app was made by principals, and all students in a school either get the app or don't, then select the \"School ID\" variable or some other indicator of which school the student attended. Similarly, if the intervention is at the classroom level, the clusters are the classrooms.  What variable, if any, should we use as a cluster indicator?",
     data.target = "E", 
     action = html_action(
      uiOutput('ClusterVar')
     )
   ),
   grade_var = html_step(
     header = 'Should analysis be conducted by grade?',
     subheader = 'In some cases it is important that analysis be conducted within groups of students in the same grade. If this does not matter for your evaluation, leave this as "combine all grades" in the select menu. If you want to analyze students separtely by grade, please select the name of the variable that indicates student grades.',
     data.target = "F", 
     action = html_action(
       uiOutput('grade_var')
     )
   ),
   credible_interval = html_step(
     header = 'Credible interval',
     subheader = uiOutput('Q_BD_3'),
     data.target = "G",    
   ),
   analyze_button = actionButton(inputId = "go", label = "Run Analysis!"),
   output_by_grade = uiOutput('output_by_grade')
)

