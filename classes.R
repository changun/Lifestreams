# refClass for project
Project <- setRefClass(
  "Project", 
  fields = c("name", "users", "features", "dat", "rawDat", "vizs"),
  methods = list(
    # get user by username
    getUser=function(user.id){
      for(user in users){
        if(user$id==user.id)
          return(user)
      }
      return(NULL)
    }
    
    
  )
)

# refClass for participant
User <- setRefClass(
    "User", 
    fields = c("id")
)
# refClass for features
# type: should be: numeric, ordinal, or categorical
# source: survey data or mobility, or audiosense data
Feature <- setRefClass(
  "Feature", 
  fields = c("name", 
             "type",
             "source",
             "group",
             "description",
             "timeInterval")
)
# refClass for the data table. 
# which stores the data (i.e. a collection of features) of each users
# This class shall support most common queries e.g. return X user's Y features in Z interval
# features: a list of features objects
# users: a list of user objects
# dat: a data table objectinitFields
DataTable <- setRefClass(
  "DataTable", 
  fields = c("lastUpdated", "fetures", "users", "dat")
)

# refClass for visualization 
Viz <- setRefClass(
  "Viz", 
  fields = c("name", "inputPanel", "outputPanel", "outputFunctions")
)