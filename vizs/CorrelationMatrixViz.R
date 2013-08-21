library(shiny)
library(PerformanceAnalytics)
library(reshape)
CorrelationMatrixViz.getNumericFeatures <- function(){
  numericFeatures <- laply(project$features, function(f){
    if(f$type %in% c("numeric","ordinal"))
      return(f$name)
    return(NA)
  })
  
  numericFeatures[!is.na(numericFeatures)]
}


inputPanel <- expression(list(

  sliderInput("correlation_threshold", "Correlation Level Threshold", 0.1, 0.9, 0.5, step = 0.1),
  # select correlation
  selectInput("correlation_method", "Choose a correlation estimation method:", 
              choices = c("pearson", "kendall", "spearman")),
  tags$head(
    tags$style(type="text/css", paste("#correlation_features { min-height: ", max(600,length(CorrelationMatrixViz.getNumericFeatures())*20),"px; }", sep=""))
  ),  
  #sliderInput("correlation_significance", "Correlation Significance Threshold (%)", 85, 99, 95, step = 1),
 checkboxInput("correlation_interuser", "Evaluate Inter-user Correlation"),
  
  # select user
  selectInput("correlation_users", "Choose a user:", 
              choices = unique(names(project$users)), multiple = T),
  # select feature
  selectInput("correlation_features", label = "Please Select Features", 
              choices = CorrelationMatrixViz.getNumericFeatures(), selected = NULL, multiple = T)
))

outputPanel <- expression(list(plotOutput("correlation.detailPlot", height = "1000px")))

outputFunction <- list(
  correlation.detailPlot = expression(renderPlot({ 
    users <- input$correlation_users
    features <- input$correlation_features
    # prepare table
    if(input$correlation_interuser && length(users) > 1){
      dat <- project$dat[project$dat$user.id == users[1] ,c(features, "date")]
      names(dat) <- ifelse(names(dat)=="date", "date", paste(users[1], names(dat), sep="."))
      for(user in users[2:length(users)]){
        next_user_dat <- project$dat[project$dat$user.id == user ,c(features, "date")]
        names(next_user_dat) <- ifelse(names(next_user_dat)=="date", "date", paste(user, names(next_user_dat), sep="."))
        dat <- merge(next_user_dat, dat, all=T)
      }
      dat <- dat[, names(dat)!="date"]
      dat <- data.matrix(dat)
    }
    else if( length(features) >1){
      dat <- data.matrix(project$dat[project$dat$user.id %in% users ,features])
    }
    else{
      return(NULL)
    }
    
    print(dat)
    # perform correlation analysis
    correlations <- abs(cor(dat,use="pairwise.complete.obs"))
    diag(correlations) <- 0
    highCorrelationFeatures <- colnames(dat)[apply(correlations, 1, max, na.rm=T) >= input$correlation_threshold]
    dat <- data.matrix(dat[ ,highCorrelationFeatures])
    if(ncol(dat)>1)
      print(chart.Correlation(dat, use="pairwise.complete.obs", method=input$correlation_method))
    
      
    
    
  
}, height=function(){1000})))



correlationMatrixViz <- Viz$new(name="CorrelationMatrix", inputPanel=inputPanel, outputPanel=outputPanel, outputFunctions=outputFunction)
