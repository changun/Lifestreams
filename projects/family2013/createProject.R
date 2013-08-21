family2013.fixPromptNames <- function(dat){
  names(dat) <- gsub("prompt.id.", "", names(dat))
  names(dat) <- gsub("parentChild", "WithEachOther", names(dat), ignore.case = T)
  names(dat) <- gsub("Child", "TheOther", names(dat), ignore.case = T)
  names(dat) <- gsub("Parent", "TheOther", names(dat), ignore.case = T)
  names(dat) <- gsub("interactionEvening", "interactionDetailsEvening", names(dat), ignore.case = T)
  return(dat)
}

# create project
createProject <- function(){
  parentDat <- family2013.fixPromptNames(oh.survey_response.read("urn:campaign:emaParentv6"))
  childDat <- family2013.fixPromptNames(oh.survey_response.read("urn:campaign:emaChildv6"))
  
  
  # create features  
  features <- createProjectUtils.createSurveyFeatures(childDat, "urn:campaign:emaChildv6")
  
  # all our features are daily
  for(f in features){
    f$timeInterval <- "Daily"
  }
  # create users
  users <- c(createProjectUtils.createUsers(childDat),
                       createProjectUtils.createUsers(parentDat))
    # add an anditional field to identify who is parent/child
  FamilyUser <- setRefClass("FamilyUser",
                            contains = c("User"),
                            fields = c("type"))
  users <- llply(users, function(u){
    FamilyUser$new(id=u$id, 
                   type=ifelse(u$id %in% parentDat$user.id, "Parent", "Child"))
  })
  
  # create dat
  rawDat <- createProjectUtils.rbindWithAttributes(parentDat,childDat)
  # transform to daily dat
  dailyDat <- createProjectUtils.transformToDailyFeatureTable(rawDat)
  # get audio probe data
  # audioDat <- observerAPI.audioSensProbe(users)
  # generate daily audio summary
  audioFeatures <- ddply(audioDat, .(user.id, date), function(d){
    speech = d$count_speech
    silent = d$count_silent
    if ((sum(speech) + sum(silent)) / (24*60) < 0.7 )
      return(NULL)

    scaler <- sum(speech) + sum(silent)
    print(sum(speech) / scaler)
    print(silent)
    print(scaler)
    data.frame(dailySpeech=sum(speech) / scaler,
               dailySilent= sum(silent) / scaler,
               moringSpeech = sum(speech[d$hour>= 6 & d$hour < 12 ]) / scaler,
               afternoonSpeech = sum(speech[d$hour>= 12 & d$hour < 18 ]) / scaler,
               eveningSpeech = sum(speech[d$hour>= 18 & d$hour <= 24]) / scaler)
  })
  
  #audioFeatures$coverage <- audioFeatures$dailySpeech + audioFeatures$dailySilent
  #audioFeatures$dailySpeech <- audioFeatures$dailySpeech / audioFeatures$coverage 
  #audioFeatures$dailySilent<- audioFeatures$dailySilent / audioFeatures$coverage 
  
  # TODO: keep all.y also
  dailyDat <- merge(dailyDat, audioFeatures, by= c("user.id", "date"), all.x=T)

  # add audio features
  features <- c(features, list(dailySpeech=Feature$new(name="dailySpeech", type="numeric", source="audioSense", timeInterval="daily", description="Daily speech time detected by audioSense"),
                               dailySilent=Feature$new(name="dailySilent", type="numeric", source="audioSense", timeInterval="daily", description="Daily silence time detected by audioSense"),
                               moringSpeech=Feature$new(name="moringSpeech", type="numeric", source="audioSense", timeInterval="daily", description="Daily silence time detected by audioSense"),
                               afternoonSpeech=Feature$new(name="afternoonSpeech", type="numeric", source="audioSense", timeInterval="daily", description="Daily silence time detected by audioSense"),
                               eveningSpeech=Feature$new(name="eveningSpeech", type="numeric", source="audioSense", timeInterval="daily", description="Daily silence time detected by audioSense")))
  # project class with audioDat field
  FamilyWellnessProject <- setRefClass("FamilyWellnessProject",
    contains = c("Project"),
    fields = c("audioDat", "dailyDat")
  )
  # create project object
  project <- FamilyWellnessProject$new(name="FamilyWellness2013",
                                       features=features, 
                                       users=users, 
                                       dat=dailyDat, 
                                       dailyDat=dailyDat,
                                       rawDat=rawDat,
                                       audioDat=audioDat, 
                                       vizs=list(dummyViz,changeDetectionViz, correlationMatrixViz))
  project
}
