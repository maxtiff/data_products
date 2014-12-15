
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Shiny Test: Diabetes Prediction"),

  # Sidebar with a numeric input

    sidebarPanel(
      numericInput('glucose', 'Glucose mg/dl', 90,
                   min  = 50,
                   max  = 200,
                   step = 5),
      submitButton('Submit')
    ),

    # Show the prediction
    mainPanel(
      h3('Results of prediction'),
      h4('You entered'),
      verbatimTextOutput("inputValue"),
      h4('Which resulted in a prediction of '),
      verbatimTextOutput('prediction')
    )
  )
)
