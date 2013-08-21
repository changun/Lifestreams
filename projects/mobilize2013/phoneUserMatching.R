require(RMySQL)
require(RCurl)
require(rjson)
require(stringr)
require(plyr)
require(doMC)
require(foreach)
registerDoMC(8)
getMobilizeUsers <- function(){
  mydb = dbConnect(MySQL(), user='root', dbname='service', host='systemsens.ohmage.org',password="Cens0123!" )
  # all mobilize user shold use systemsens with ver. of 4.21
  # Scott's development is 4.1 (but some user use 4.21.....)
  rs = dbGetQuery(mydb, "select phone_model, version, imei, last_upload 
                  from visualization_client
                  where version='4.21'
                  ")
  #   rs$time <- 0
  #   for(imei in rs$imei){
  #     # aggregate the samples by 10 mins
  #     dat = dbGetQuery(mydb, paste("select dt_record from systemsens_", imei, sep=''))
  #     timeframes <- unique(as.integer(as.integer(as.POSIXct(dat$dt_record)) / (60*10)) * (60*10))
  #     rs[rs$imei==imei,]$time <- length(timeframes) * 10
  #   }
  rs$cens_phone <- F
  cens_phones <- as.character(read.csv("mobilize_phones_2013.csv")$imei)
  rs[rs$imei %in% cens_phones,]$cens_phone <- T
  # the cens phones that does not in systemsens db
  rs_cens <- data.frame(imei=cens_phones[!cens_phones %in% rs$imei], 
                        version='4.21', 
                        last_upload=NA, 
                        phone_model='HTC - myTouch_4G_Slide-htc_doubleshot')
  rbind(rs,rs_cens)
}
getSystemsensActivityLog <- function(imei){
  mydb = dbConnect(MySQL(), user='root', dbname='service', host='systemsens.ohmage.org',password="Cens0123!" )
  # aggregate the samples by 10 mins
  dat = dbGetQuery(mydb, paste("select * from systemsens_", imei, 
                               " where data_type='activitylog'", sep=''))
  systemsens_activity_log<- data.frame()
  for(i in 1:nrow(dat)){
    logs = fromJSON(dat$message[i])$data
    
    for(log in logs){
      if(!is.null(log$activity) && 
           str_detect(log$activity,fixed("ohmage"))){
        activity <- tail(str_split(log$activity, '\\.')[[1]], 1)
        time_stamp <-log$time_stamp
        event <-log$event
        systemsens_activity_log <- rbind(systemsens_activity_log, data.frame(activity=activity,
                                                                             time_stamp=as.POSIXct.numeric(time_stamp/1000000000.0, origin="1970-1-1", tz="GMT"),
                                                                             event=event))
      }
    }
  }
  dbDisconnect(mydb)
  return(systemsens_activity_log)
  
}

getActivityLog <- function(user, stream_version=2){
  token <- getToken()
  res <- postForm("https://lausd.mobilizingcs.org/app/stream/read",.opts=curlOptions(ssl.verifypeer=F),
                  client='ohmageEasyPost',
                  auth_token=token,
                  observer_id='org.ohmage.LogProbe',
                  stream_version=stream_version,
                  stream_id='activity',
                  username = user,
                  style = "POST")
  json_data <- fromJSON(res)
  probe_log_dat <- ldply(json_data$data, function(entry){
    data.frame(activity=entry$data$activity, time=entry$metadata$timestamp)
  })
  # convert date string to POSIXct
  probe_log_dat$seconds <- as.integer(as.POSIXct(gsub("T", " ",probe_log_dat$time), tz="GMT"))
  probe_log_dat
}

getAllUsers <- function(){
  token <- getToken()
  res <- postForm("https://lausd.mobilizingcs.org/app/user/search",.opts=curlOptions(ssl.verifypeer=F),
                  client='ohmageEasyPost',
                  auth_token=token,
                  
                  style = "POST")
  json_data <- fromJSON(res)
  names(json_data$data)
}
getToken <- function(){
  res <- postForm("https://lausd.mobilizingcs.org/app/user/auth_token",.opts=curlOptions(ssl.verifypeer=F),
                  user="mobilize.dev",
                  password="Hedgehog!567", 
                  client="ohmageEasyPost",   style = "POST")
  json_data <- fromJSON(res)
  json_data$token
}
# Generate the IMEI <-> Ohmage username mapping

# get all the acitivty log
users <- getAllUsers()
activity_log <- list()
for(user in users){
  print(user)
  activity_log[[user]] <- getActivityLog(user,2)
  print(nrow(activity_log[[user]]))
}


# get all the systemsens log
systemsens_log <- list()
for(imei in rs$imei){
  print(imei)
  systemsens_log[[imei]] <- getSystemsensActivityLog(imei)
}



matches <- ldply(activity_log, function(probe_log){
  # convert the log to data frame
 if(nrow(probe_log) == 0)
   return(NULL)
 probe_log$seconds = as.integer(probe_log$seconds / 120) * 120
  ret <- data.frame()
  # go over the log of every phone
  for(imei in names(systemsens_log)){
    sense_log_dat <- systemsens_log[[imei]]
    sense_log_dat$seconds <- as.integer(sense_log_dat$time_stamp)
    sense_log_dat$seconds =  as.integer(sense_log_dat$seconds / 120) * 120
    matchPercentage = nrow(probe_log[probe_log$seconds %in% sense_log_dat$seconds,])/ nrow(probe_log)
    ret <- rbind(ret, data.frame(match=matchPercentage, imei=imei))
  }
  return(ret)
}, .parallel=T)
dat <-ldply(systemsens_log, data.frame)

