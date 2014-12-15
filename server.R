
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

diabetesRisk <- function(glucose) glucose/200

detectOutliers <- function(data,plot=TRUE) {

  ## Read in data.
  x <- as.ts(data)

  ## Estimate trend and components and take residual.
  if(frequency(x)>1)
    resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
  else
  {
    tt <- 1:length(x)
    resid <- residuals(loess(x ~ tt))
  }

  ## Break into quantiles for outlier detection and score observations on severity.
  resid.q <- quantile(resid,prob=c(0.25,0.75))

  ## Establish interquantile range
  iqr <- diff(resid.q)
  limits <- resid.q + 2.5*iqr*c(-1,1)

  ## Determine score of data point. Only examine scores above 1.
  score <- floor(abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0)))

  ## Plot outliers on TS graph.
  if(plot)
  {
    plot(x)
    x2 <- ts(rep(NA,length(x)))
    x2[score>0] <- x[score>0]
    tsp(x2) <- tsp(x)
    points(x2,pch=19,col="red")

    #   plot(adore.filter(x,min.width = 5,max.width = 10,rtr=1))

    return(invisible(score))

  }
  ## Check value of score.
  else if (sum(score) > 0) {

    return(score)

  } else {

    return("No anomalies have been detected.")

  }

}

shinyServer(
  function(input, output) {

  output$inputValue <- renderPrint({input$glucose})
  output$prediction <- renderPrint({diabetesRisk(input$glucose)})

  }
)
