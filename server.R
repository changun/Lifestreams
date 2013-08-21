library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  output$inputPanel <- renderUI({
    if(!"tabset" %in% names(input))
      return(NULL)
    for(viz in project$vizs){
      if(viz$name == input$tabset){
        return(do.call(tagList, eval(viz$inputPanel)))
      }
    }
  })
  
  output$tabsetPanel <- renderUI({
    # create tabpanel as specified in project$vizs
    tabPanels <- llply(project$vizs, function(viz){
      do.call(tabPanel, c(viz$name, eval(viz$outputPanel)))
    })
    print(tabPanels)
    # create tabpanel set with id "tabset"
    do.call(tabsetPanel, c(list(id="tabset"), tabPanels))
    
  })
  for(viz in project$vizs){
    for(funcname in names(viz$outputFunctions)){
      output[[funcname]] <- eval(viz$outputFunctions[[funcname]])
    }
  }
  print(output)

})