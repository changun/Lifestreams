library(shiny)
inputPanel <- expression(list(sliderInput("Dummy.obs", 
                                   "Number of observations:", 
                                   min = 1,
                                   max = 1000, 
                                   value = 500))
)


outputPanel <- expression(list(plotOutput("Dummy.distPlot")))

outputFunction <- list(Dummy.distPlot = expression(renderPlot({ 
  # generate an rnorm distribution and plot it
  dist <- rnorm(input$Dummy.obs)
  hist(dist)
})))



dummyViz <- Viz$new(name="Dummy", inputPanel=inputPanel, outputPanel=outputPanel, outputFunctions=outputFunction)