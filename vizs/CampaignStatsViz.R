library(shiny)
inputPanel <- expression(list(
  # select user
#   selectInput("CampaignStatsViz_user", "Choose a user:", 
#               choices = unique(names(project$users))),
  tableOutput("CampaignStatsViz_overallStatistics")
  
  )
)


outputPanel <- expression(list(tableOutput("CampaignStatsViz_features")))

outputFunction <- list(
  
  CampaignStatsViz_overallStatistics = expression(renderTable({ 
    # compute campaign stats
    stats <- list()
    
    stats[["# Users"]] <- length(project$users)
    stats[["# Survey Answered"]] <- nrow(project$rawDat)
    stats[["# Prompts Answerd"]] <-  sum(!is.na(project$rawDat))
    
    stats[["# Features"]] <- length(project$features)
    stats[["Since"]] <- as.character(project$dat$date[1])
    stats[["To"]] <- as.character(tail(project$dat$date,1))
    mStats <- matrix(stats)
    rownames(mStats) <- names(stats)
    return(mStats)
  })),
  CampaignStatsViz_features = expression(renderTable({      
    ldply(project$features, function(f){
      print(f)
      data.frame(name=f$name, 
                 description=f$description, 
                 points=sum(!is.na(project$dat[[f$name]])))
    })
    
  }))
                              
                              

)



CampaignStatsViz <- Viz$new(name="Campaign Statistics", inputPanel=inputPanel, outputPanel=outputPanel, outputFunctions=outputFunction)