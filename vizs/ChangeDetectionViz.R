library(shiny)
library(lubridate)
inputPanel <- expression(list(
  # select user
  selectInput("changeDetection_user", "Choose a user:", 
              choices = unique(names(project$users))),
  # select feature
  selectInput("changeDetection.features", label = "Please Select Features", 
              choices = names(project$features), selected = NULL, multiple = T),
  # change detection filter
  checkboxInput("changeDetection.changeFilter", "Show All Features With Changes",T),
  # advanced parameters
  checkboxInput("changeDetection_advancedOpts", "Advanced Options"),
  conditionalPanel(
    condition = "input.changeDetection_advancedOpts == true",
    # choose day type
    selectInput("changeDetection.dayType", label= 'Day Type', 
                choices=c('Weekday','Weekend'), 
                selected=c('Weekday','Weekend'), multiple=TRUE),
    # change detection parameters
    selectInput("changeDetection.ARL", "ARL", choices = c(370,500,600, 700, 1000, 2000, 3000, 10000,20000, 50000) ),
    sliderInput("changeDetection.startup", "Startup", 20, 60, 30, step = 5)
  )


))

outputPanel <- expression(list(uiOutput("changeDetection.plots")))


plotChanges <-function(user, feature, wdays, ARL0, startup){
  # get the data
  dat <- project$dat[project$dat$user.id==user$id & wday(project$dat$date) %in% wdays, 
                     c(feature$name, "date")]
  # filter ot NAs
  dat <- dat[!is.na(dat[[feature$name]]),]
  # plot
  d <- ggplot(dat,aes_string(x="date", y=feature$name))
  d <- d + geom_point(colour="black",pch=21, size=4, fill=sample(2:20,1))
  # set x range so every plot has the same time range
  d <- d + xlim(min(project$dat[project$dat$user.id==user$id,]$date), 
            max(project$dat[project$dat$user.id==user$id,]$date))
  # set x tick font
  d <- d + theme(axis.text.x = element_text(size=15))
  # set y tick font
  d <- d + theme(axis.text.y =element_text(size=12))
  # set y title font
  d <- d + theme(axis.title.y =element_text(face="bold", size=16))
  change_dates <- getChangePoints(user, feature, wdays, ARL0, startup)
  if(!is.null(change_dates)){
    d <- d + geom_vline(aes(xintercept = as.integer(changePoint)), data=change_dates, size=0.7) 
    d <- d + geom_vline(aes(xintercept = as.integer(detectionTimes)), data=change_dates, linetype = "dashed")
  }

  return(d)
}

outputFunction <- list(changeDetection.plots = expression(
  renderUI({
      # get user object
      user = project$getUser(input$changeDetection_user)
      # get the days in a week to display
      wdays = c()
      if("Weekday" %in% input$changeDetection.dayType)
        wdays <- c(2:6,wdays)
      if("Weekend" %in% input$changeDetection.dayType)
        wdays <- c(1,7,wdays)

      # create output function's and plot object
      plot_output_list <- llply(project$features, function(feature) {
        if(feature$type %in% c("logical", "numeric", "ordinal") &&
          feature$name %in% input$changeDetection.features){
          plotname <- paste("plot", user$id, feature$name, input$changeDetection.dayType, sep=".", collapse=".")
          
          
          output[[plotname]] <- renderPlot({
            print(plotChanges(user, feature,  wdays,
                              input$changeDetection.ARL0, input$changeDetection.startup))
          })
          return(plotOutput(plotname))
        }
      })
      # filter out null values from the plot list
      for(name in names(plot_output_list)){
        if(is.null(plot_output_list[[name]]))
          plot_output_list[[name]] <- NULL
      }
      # return output plot
      do.call(tagList, plot_output_list)
  })
    

  )
)


changeDetectionViz <- Viz$new(name="ChangeDetection", 
                    inputPanel=inputPanel, 
                    outputPanel=outputPanel, 
                    outputFunctions=outputFunction
            )
