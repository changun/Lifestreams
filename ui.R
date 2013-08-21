library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Project title
  headerPanel(project$name),
  
  # Sidebar content is dynamically genereted by the selected viz object
  sidebarPanel(
    includeHTML("www/js/tools.js"),
    
    uiOutput("inputPanel")
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    uiOutput("tabsetPanel")
  )
))