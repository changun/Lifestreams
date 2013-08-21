require(memoise)
require(glm2)
require(MCMCglmm)
require(BayesFactor)
require(lme4)
require(MASS)
getTotalPhoneOnTime <- function(imei, begin=NULL, end=NULL){
  records = systemsensAPI.dailyPowerOnTime(imei, begin, end)
  if(is.null(records) || nrow(records)==0)
    return(0)
  sum(records$CPUOnTime)
}
mSystemsensAPI.screenEvents <- memoise(systemsensAPI.screenEvents)
getTotalScreenOnTime <- function(imei, begin=NULL, end=NULL){
  records = systemsensAPI.screenEvents(imei, begin, end)
  if(is.null(records) || nrow(records)==0)
    return(0)
  records$date <- as.Date(records$on)
  dat <- ddply(records, .(date), function(dailyRecords){
    data.frame(screenOnTime=(sum(as.POSIXct(dailyRecords$off) - as.POSIXct(dailyRecords$on))))
  })
  sum(dat$screenOnTime)
}
getMeanInteractionTime<- function(imei, begin=NULL, end=NULL){
  records = systemsensAPI.screenEvents(imei, begin, end)
  if(is.null(records) || nrow(records)==0)
    return(0)
  return(mean(as.POSIXct(records$off) - as.POSIXct(records$on)))
}

getAppUsed <- function(imei, begin=NULL, end=NULL){
  records <- systemsensAPI.activityLog(imei)
  if(is.null(records) || nrow(records)==0)
    return(0)
  length(unique(records$appname))
}

getCameraUsed <- function(imei, begin=NULL, end=NULL){
  dat <- systemsensAPI.activityLog(imei)
  if(is.null(dat) || nrow(dat)==0)
    return(0)
  dat <- dat[order(dat$time_stamp),]
  camera_events <- dat[dat$activity=="com.android.camera/.CameraEntry" &
        (!dat$event %in% c("am_destroy_activity", "am_pause_activity")),]
  nrow(camera_events)
}


# create project
createProject <- function(){
  campaigns <- oh.campaign.read("long")
  # get all 2013 campaigns
  campaignNames <- names(campaigns$data)
  campaignNames2013 <- campaignNames[grepl("2013", campaignNames) & !grepl("demo|Seminar", campaignNames) ]
  # compute the number of survey answered by each student,
  # and the begin & end dates of the campaign estimated by the first and last survey dates
  user_survey_count <- ldply(campaignNames2013,  function(campaign){
    campaign_dat <- oh.survey_response.read(campaign)
    campaign_dat <- campaign_dat[grepl("2013", campaign_dat$user.id),]
    if(nrow(campaign_dat) < 4){
      print(campaign)
      return(NULL)
    }
    users <- oh.survey_response.read(campaign, to.data.frame=F)$data[["urn:ohmage:user:id"]]$values
    if(nrow(campaign_dat) > 0){
      student_survey_count <- ddply(campaign_dat, .(user.id), nrow)
      no_response_users <- unique(users[!(users %in% student_survey_count$user.id)])
      student_survey_count <- rbind(student_survey_count, 
                                    data.frame(user.id=no_response_users, V1=rep(0,length(no_response_users))))
      survey_timestamp <- as.POSIXct(campaign_dat[["context.timestamp"]])
      survey_timestamp <- survey_timestamp[survey_timestamp > as.POSIXct('2013-1-1')]
      student_survey_count$firstInClass <- min(survey_timestamp)
      student_survey_count$lastInClass <- max(survey_timestamp)
      classes <- campaigns$data[[campaign]]$classes
      classes <- classes[!grepl("demo", classes) & !grepl("urn:class:mobilize:", classes) ]
      student_survey_count$class <- classes[1]
      student_survey_count$campaign <- campaign
      student_survey_count
    }
  })
  # aggreagte data  from multiple campaigns for each user
  merged_user_survey_count <- ddply(user_survey_count, .(user.id), function(d){
    if(!grepl("2013",d$user.id[1]))
      return(NULL)
    if(length(unique(d$class)) > 1){
      return(NULL)
      #stop("some student belongs to more than one class")
    }
    data.frame(survey_count=sum(d$V1), firstInClass=min(d$firstInClass), lastInClass=max(d$lastInClass), class=d$class[1], numOfCampaign=nrow(d), campaigns=paste(unique(d$campaign), collapse=" "))
  })
  d <- merged_user_survey_count[merged_user_survey_count$class %in% unique(user_stats$class)[c(1,2,3, 8)], ]
  d
  d$class <- gsub("(urn:class:)|(ca:)|(:SP2013)", "", d$class)
  ggplot(d, aes(x=class, y=survey_count, fill=class)) + geom_boxplot(alpha=0.5)+geom_point() + geom_jitter()
  # import user-imei mapping
  mapping <- read.csv("username_phone_mapping_2013.csv")
  names(mapping) <- c("user.id", "Asset")
  phones <- read.delim("mobilize_phones.txt", sep=" ")
  phone_user_mapping <- merge(phones,mapping)
  phone_user_mapping$user.id <- gsub("lausd", "2013", phone_user_mapping$user.id)
  
  # merge the user-imei mapping with survey count table
  user_survey_count_withIMEI <- merge(merged_user_survey_count, phone_user_mapping)
  user_survey_count_withIMEI$IMEI <- as.character(user_survey_count_withIMEI$IMEI)
  # get features from systemsens data
  user_stats <- ddply(user_survey_count_withIMEI, .(user.id), function(row){
    gc()
    begin <- row$fisrtInClass - as.difftime(7, unit="days")
    end <- row$lastInClass + as.difftime(7, unit="days")
    
    row$totalPhoneOnTime <- getTotalPhoneOnTime(row$IMEI, begin, end)
    if(row$totalPhoneOnTime == 0)
      return(NULL) # only return those phone we got systemsens data
    row$totalScreenOnTime <- getTotalScreenOnTime(row$IMEI, begin, end)
    row$meanInteractionTime <- getMeanInteractionTime(row$IMEI, begin, end)
    row$appUsed <- getAppUsed(row$IMEI, begin, end)
    row$cameraUsed <- getCameraUsed(row$IMEI, begin, end)
    network <- systemsensAPI.totalNetworkUsage(row$IMEI, begin, end)
    row$totalNetworkUsage <- sum(network[,c("Rx", "Tx")])
    row$totalMobileUsage <- sum(network[,c("MobileRx", "MobileTx")])
    row
  }, .parallel=T)

  dat <- user_stats[user_stats$totalPhoneOnTime >0, ]
  dat$class <- gsub("(urn:class:)|(ca:)|(:SP2013)", "", dat$class)
  dat$days <- (as.integer(dat$lastInClass - dat$firstInClass) +1)
  dat$totalNetworkUsage <- user_stats_network$totalNetworkUsage
  dat$totalNetworkUsage <- user_stats_mobile$totalNetworkUsage
  
  dat$wifiUsage <-  dat$totalNetworkUsage - dat$totalMobileUsage 
  a <- lmer(survey_count~log(meanInteractionTime) + log(totalNetworkUsage) + (1|class), family=poisson, data=dat)
  b <- lmer(survey_count~1 + (1|class), family=poisson, data=dat)
  
  a <- lme(survey_count~log(meanInteractionTime) + log(totalNetworkUsage), random= ~1|class, data=dat)
  
  a <- summary(MCMCglmm(survey_count~class, family="poisson", data=dat))
  
  
  p <- ggplot(dat2[dat2$class=="STEAM:ECS_P5", ], aes(x=log(totalNetworkUsage), y=log(meanInteractionTime), colour=survey_count, size=survey_count, label=survey_count)) +
       geom_point() + 
       scale_size(range = c(3, 11)) +
       geom_text(size=6, mapping=aes(x=log(totalNetworkUsage)+0.25)) + guides(size=FALSE, colour=F)
  direct.label(p)
  p <- ggplot(dat2, aes(y=log(survey_count), x=log(meanInteractionTime), colour=survey_count, size=survey_count)) + geom_point() + scale_size(range = c(3, 10)) + facet_grid(class~.) +    geom_smooth(method=lm) 
  p <- ggplot(dat2[dat2$class=="SouthGate:ECS_P2",], aes(y=log(survey_count), x=log(meanInteractionTime), colour=survey_count, size=survey_count)) + 
       geom_point() + 
       scale_size(range = c(3, 10)) +
       geom_smooth(method=lm, se=F)+
       geom_abline(aes(intercept=2.447481,slope=0.1023199))
  
  
  direct.label(p)
  p <- ggplot(dat2, aes(x=log(totalNetworkUsage), y=log(meanInteractionTime), colour=survey_count, size=survey_count)) + geom_point() + scale_size(range = c(3, 10)) + facet_grid(class~.)
  direct.label(p)
  
  ggplot(dat, aes(x=fitted(a), y=dat2$survey_count)) + geom_point()
  summary(lm(survey_count~class, dat))
  ggplot(dat, aes(avgSurveyCount, alpha=0.2, fill=class)) + geom_density()
  summary(aov((survey_count/days)~class, dat))
  summary(glm(survey_count~1, family=poisson, data=dat))
  summary(glm(survey_count~log(totalMobileUsage), family=poisson, data=dat))
  lmer(survey_count~log(appUsed) + (1|class), family=poisson, data=dat)
  a <- lmer(survey_count~log(meanInteractionTime) + log(totalNetworkUsage) +  (1|class), family=poisson, data=dat)
  lmer(survey_count~log(totalNetworkUsage+1) + (1|class), family=poisson(link = "log"), data=dat)

  
  glmer(survey_count~days+(1|class), family=poisson, data=dat)

  glFit7 <- glmer(survey_count~1 +(1|class), family=poisson, data=dat)
  summary(glm(survey_count~as.integer(days), family=poisson, data=dat))
  qqmath(ranef(glFit6, postVar = TRUE), strip = FALSE)
   glFit3
  anova(glFit1, glFit2, glFit3, glFit4, glFit5, glFit6, glFit7, glFit8)
  summary(MCMCglmm(survey_count~avgNetworkUsage+days, ~class, data=dat, family="poisson"))
  plot(aov(avgSurveyCount~class+, dat))

  names((oh.survey_response.read("urn:campaign:lausd:UCLACommunity:SP2012:ECS_P6:Snack"))
  parentDat <- family2013.fixPromptNames(oh.survey_response.read("urn:campaign:lausd:UCLACommunity:SP2012:ECS_P6:Snack"))
  childDat <- family2013.fixPromptNames(oh.survey_response.read("urn:campaign:emaChildv6"))
  
  oh.campaign.read()
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
