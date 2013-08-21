require(RCurl)
require(rjson)

observerAPI.queryObserverData <- function(observer_id, stream_id, stream_version, user.id){
  res <- postForm(paste(getOption("SERVERURL"), "/stream/read", sep=""), .opts=curlOptions(ssl.verifypeer=F),
                  .params = list(
                    client='lifestreams',
                    auth_token=getOption("TOKEN"),
                    observer_id=observer_id,
                    stream_version=stream_version,
                    stream_id=stream_id,                  
                    username =  user.id
                  )
  )
  json_dat <- fromJSON(res)
  if(!is.null(json_dat$metadat) && json_dat$metadata$count == 2000)
    stop("potential observer API bug")
  if(json_dat$result=="failure")
    stop("ObserverAPI query failed")
  return(json_dat)
}

# get audioSens dat from ohmage server
# users: a list of user object
observerAPI.audioSensProbe <- function(users){
  audioDat <- ldply(users, function(user){
    dat <- observerAPI.queryObserverData(observer_id='org.ohmage.probes.audioSensProbe',
                                  stream_id='summarizers',
                                  stream_version=14, 
                                  user.id=user$id)
    
    
    dat <- ldply(dat$data, function(d){
      time <- as.POSIXct(gsub("T", "",d$metadata$timestamp))
      data.frame(user.id = user$id,
                 count_total = d$data$summary$count_total,
                 count_missing = d$data$summary$count_missing,
                 count_speech = d$data$summary$count_speech,
                 count_silent = d$data$summary$count_silent,
                 time = time, date =as.Date(time, attr(time, "tzone")), hour= as.POSIXlt(time)$hour
                 )
    })
    dat[[".id"]] <- NULL
    dat
  })
  # remove dummy column
  audioDat[[".id"]] <- NULL
  audioDat
}



observerAPI.logProbeActivity <- function(user){
  json_data <- observerAPI.queryObserverData(observer_id='org.ohmage.LogProbe',
                                       stream_id='activity',
                                       stream_version=2, 
                                       user.id=user)
 
  probe_log_dat <- ldply(json_data$data, function(entry){
    data.frame(activity=entry$data$activity, time=entry$metadata$timestamp)
  })
  # convert date string to POSIXct
  probe_log_dat$seconds <- as.integer(as.POSIXct(gsub("T", " ",probe_log_dat$time), tz="GMT"))
  probe_log_dat
}

