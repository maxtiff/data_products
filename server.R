## Source all required scripts.
# required.scripts <- c('global.R','novelty_detector.R')
# sapply(required.scripts, source, .GlobalEnv)

## Load required libraries
library(shiny)
# library(rCharts)
# library(ggvis)

# Define server logic for random distribution application
shinyServer(function(input, output) {


  # Reactive expression to generate the requested distribution. This is
  # called whenever the inputs change. The output renderers defined
  # below then all used the value computed from this expression
  data <- reactive({
    input$goButton
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)

    dist(input$n)
  })


  # output$dateRangeText  <- renderText({
  #   paste("input$dateRange is",
  #     paste(as.character(input$dateRange), collapse = " to ")
  #   )
  # })

  # Generate a plot of the data. Also uses the inputs to build the
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    input$goButton
    plot(data(),
         main=paste('r', dist, '(', n, ')', sep=''))
  })

  # Generate a summary of the data
  output$summary <- renderPrint({
    input$goButton

    summary(data())
  })

  # Generate an HTML table view of the data
  output$table <- renderTable({
    input$goButton

    data.frame(x=data())
  })
})