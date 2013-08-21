# create a list of features from a data table returned from ohmage
createProjectUtils.createSurveyFeatures <- function(dat, campaignName){
  # query campaign again for prompt discription
  prompt_dat <- oh.survey_response.read(campaignName, to.data.frame=F, prompt_id_list="urn:ohmage:special:all",)$data
  # remove the urn prefix for ease of matching
  names(prompt_dat) <- gsub("urn:ohmage:prompt:id:", "", names(prompt_dat)) 
  
  
  features <- list()
  names(dat) <- gsub("prompt.id.", "", names(dat))
  featureNames <- names(dat)
  for(feature in featureNames[!featureNames %in% c("user.id", "context.timestamp")]){
    type = NULL
    classes = attr(dat[[feature]], "class")
    if('factor' %in% classes){
      if('ordered' %in% classes)
        type = "ordinal"
      else if(length(levels(dat[[feature]])) == 2)
        type = "logical"
      else
        type = "categorical"
    }
    else if('multifactor' %in% classes)
      type = "multifactor"
    else if('POSIXct' %in% classes)
      type = "datetime"
    else if(attr(dat[[feature]], "prompt_type") == "number")
      type = "numeric"
    if(is.null(type)) stop("unknown feature type")
    
    features[[feature]] = Feature$new(name = feature, type=type, source="survey")
    if( feature %in% names(prompt_dat) && ! is.null(prompt_dat[[feature]]$context$text))
      features[[feature]]$description <- prompt_dat[[feature]]$context$text
    else{
      features[[feature]]$description <- "None"
    }
    
      
  }
  features
}
# create a list of unique users from data table returned from Ohmage
createProjectUtils.createUsers <- function(dat){
  users = list()
  for(username in unique(dat$user.id)){
    users[[username]] = User$new(id=username)
  }
  return(users)
}
# rbind two data table from Ohmage while maintained the attributes of each column
createProjectUtils.rbindWithAttributes <- function(dat, dat2){
  # helper function that restores the multifactor to list of labels
  multifactorToList <- function(dat){
    entryList <- strsplit(dat, "\\+")
    
    labelList<-llply(entryList, function(entry){
      if(!is.na(entry[1]) && !entry[1]=='NA'){
        ret <- list()
        for(sel in entry){
          ret <- c(ret, attr(dat, "labels")[[as.integer(sel)]])
        }
      }
      else{
        ret = NA
      }
      ret
    })
    return(labelList)
  }
  
  if( !sum(names(dat) %in% names(dat2)) >0 )
    stop("the columns names are different")
  
  # create an empty data.frame with the combined number of rows
  mergedDat <- data.frame(matrix(NA, nrow(dat) + nrow(dat2), 0))
  
  for(feature in names(dat)){
    type = attributes(dat[[feature]])$prompt_type
    # multifactor vars have to be restored to list of labels before being combined.
    if( !is.null(type) &&  type == 'multi_choice_custom'){
      mergedValues <- c(multifactorToList(dat[[feature]]), multifactorToList(dat2[[feature]]))
      
      labels <- unique(c(unlist(mergedValues)))
      # create a combined multifactor var
      mergedDat[[feature]] <- multifactor(mergedValues, labels)
    }
    else{ # the other kinds of vars can be directly combined
      mergedValues <- c(dat[[feature]], dat2[[feature]])
      
      # set the attributes
      attributes(mergedValues) <- attributes(dat[[feature]])
      mergedDat[[feature]] <- mergedValues
    }
  }
  mergedDat 
}
# Tranform the given data table to daily feature data table
# Assume each survey shall only be answered once each day
createProjectUtils.transformToDailyFeatureTable <- function(dat){
  dat$date <- as.Date(dat$context.timestamp, tz=attr(dat$context.timestamp, "tzone"))
  dailyDat <- ddply(dat, c("user.id", "date"), function(d){
    dailyFeatures = data.frame(NA)
    for(feature in names(dat)){
      response <- tail(d[ !is.na(d[feature]), feature], 1)[1]
      if(!is.null(response))
        dailyFeatures[[feature]] <- response
      else
        dailyFeatures[[feature]] <- NA
    }
    dailyFeatures 
    
  })
  # remove the dummy column
  dailyDat[["NA."]] <- NULL
  dailyDat
}