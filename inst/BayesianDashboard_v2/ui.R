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
     title = 'Get Results: Find Out If the Technology Moved the Needle',
     subtitle = '<p><strong>Objective:</strong> In this step, you will estimate the effect of the technology. By uploading a data file and answering a few key questions, you will find an answer to the research question you crafted at the beginning of this process.</p>'
   ),
   upload_data = html_step(
     header = 'Upload your data',
     subheader = 'Upload a <a href="#"><span data-toggle="tooltip" title="To save a CSV file from Excel, go to the &quot;Save As&quot; menu, and when choosing a filename, select &quot;CSV (Comma delimited)&quot; in the &quot;Save as type&quot; selector.">CSV</span></a> file of your data. The file should have one row for each person in your sample that you collected outcome data (e.g. test scores) on, and one column for each field (for example, test score and background characteristics). Read <a target="_blank" href="https://edtechrce.org/static/pdf/04.01PrepareYourDataForAnalysis.pdf">this guide</a> for more detail about how to prepare your file. If you created a matched comparison group in the last step, you should use the file you downloaded there.
<p class="warning">Before uploading data, make sure that you have removed any personally identifiable information (PII), such as names. You may want to include a random ID that you can use to this file to link other data sources.</p>',
     data.target = "A",
     action = html_template_file_input(
      placeholder = '',
      button_text = 'Choose File',
      accept      = '.csv')
   ),
   outcome_var = html_step(
     header = 'What is the outcome variable?',
     subheader = '<p>Choose the name of the variable that measures the <a href="#"><span data-toggle="tooltip" title="Knowledge, skills, attitudes, or other desired benefits that are attained as a result of an activity.">outcome</span></a> you are studying. This may be a measure of student achievement or something like attendance.</p>',
     data.target = "B",
     action = selectizeInput(
      inputId = "outcome_var",
      label = '',
      choices = NULL,
      multiple = FALSE,
      selected = NULL,
      options = list(placeholder = "Select the outcome variable")
    )
   ),
   treatment_var = html_step(
     header = 'Who is using the technology?',
     subheader = 'Choose the name of the variable that tells us who is using the technology. Remember, this variable should equal 1 for the individuals using the technology and 0 for non-users. If you created a matched comparison group in the previous tool, use the same variable you selected there. ',
     data.target = "C",
     action = selectizeInput(
      inputId = "trt_var",
      label = '',
      choices = NULL,
      multiple = FALSE,
      selected = NULL,
      options = list(placeholder = "Select the treatment variable")
    )
   ),
   control_vars = html_step(
     header = 'Control variables, or covariates',
     subheader = '<p>What variables, if any, should we account for while estimating impacts? If you created a matched comparison group, you should include the variables used for matching in this list. </p>',
     data.target = "D",
     action = selectizeInput(
      inputId = "control_vars",
      label = '',
      choices = NULL,
      multiple = TRUE,
      selected = NULL,
      options = list(placeholder = "Select control variables")
    )
   ),
   cluster_var = html_step(
     header = 'Are individuals clustered within classrooms or schools? ',
     subheader = "If users and non-users were chosen based on groups they belonged to rather than as individuals, we need to identify these &quot;clusters&quot; in order to correctly estimate the effect of the intervention. If the decision to give students a technology was made by principals, and all students in a school either get the technology or don't, then select the &quot;School ID&quot; variable or some other indicator of which school the student attended. Similarly, if the intervention is at the classroom level, the clusters are the classrooms. What variable, if any, should we use as a cluster indicator?",
     data.target = "E",
     action = selectizeInput(
       inputId = 'cluster_var',
       label = '',
      choices = NULL,
      multiple = FALSE,
      selected = NULL,
      options = list(placeholder = "Select the cluster variable")
    )
   ),
   grade_var = html_step(
     header = 'Should the analysis be conducted by grade?',
     subheader = 'In some cases it is important that analysis be conducted within groups of students in the same grade. This may be important if the outcome measure is not comparable across grades. If this does not matter for your evaluation, leave this as &quot;combine all grades&quot; in the select menu. If you want to conduct the analysis separately by grade, please select the name of the variable that indicates student grade level.',
     data.target = "F",
     action = selectizeInput(
       inputId = "grade_var",
      label = '',
      choices = 'combine all grades',
      multiple = TRUE,
      selected = 'combine all grades',
      options = list(placeholder = "Select grade variable", maxItems = 1))
   ),
   #credible_interval = html_step(
  #   header = 'Credible interval',
  #   subheader = uiOutput('Q_BD_3'),
  #   data.target = "G"
  # ),
   diagnostic_error = htmlOutput('diagnostic_error'),
   analyze_button = actionButton(inputId = "go", label = "Run Analysis!"),
   output_by_grade = uiOutput('output_by_grade'),
   save_status = htmlOutput('save_status')
)

