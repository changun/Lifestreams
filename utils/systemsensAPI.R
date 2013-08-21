require(stringr)
require(RJSONIO)
# begin , end are inclusive
systemsensAPI.query <- function(imei, data_type, begin=NULL, end=NULL){

    mydb = dbConnect(MySQL(), dbname='service', host='systemsens.ohmage.org',
                     user=PrivateConfigurations.SYSTEMSENS_USERNAME, 
                     password=PrivateConfigurations.SYSTEMSENS_PASSWORD)
    on.exit(dbDisconnect(mydb))
  
  
  table_exist <- dbGetQuery(mydb, paste("SELECT *  FROM INFORMATION_SCHEMA.TABLES 
                              WHERE TABLE_NAME = 'systemsens_", imei, "'", sep=''))
  if(nrow(table_exist) == 0){
    
    return(NULL)
  }

  statement = paste("select * from systemsens_", imei, 
                    " where data_type='", data_type, "'", sep='')
  if(!is.null(begin)){
    statement = paste(statement, " AND dt_record >= '", begin, "'", sep='' )
  }
  if(!is.null(end)){
    statement = paste(statement, " AND dt_record <= '", end, "'", sep='' )
  }
  statement <- paste(statement, " order by dt_record")
  dat = dbGetQuery(mydb, statement)
  return(dat)
}
systemsensAPI.jsonStringsToList <- function(jsons){
  fullJsonStr = paste("[", paste(jsons, collapse=","), "]")
  fromJSON(fullJsonStr)
}
systemsensAPI.dailyPowerOnTime <- function(imei, begin=NULL, end=NULL){
  dat <- systemsensAPI.query(imei, "cpu", begin, end)
  if(is.null(dat))
    return(NULL)
  # aggreate the number of record by date
  dat$date <- as.Date(dat$dt_record)
  dat <- ddply(dat, .(date), nrow) 
  names(dat)[2] <- "CPUOnTime"
  # each record means the phone is on for 2 mins
  dat$CPUOnTime <- dat$CPUOnTime * 120

  return(dat)
}


systemsensAPI.screenEvents <- function(imei, begin=NULL, end=NULL){
  dat <- systemsensAPI.query(imei, "screen", begin, end)
  if(is.null(dat))
    return(NULL)
  events <- data.frame()
  messages <- systemsensAPI.jsonStringsToList(dat$message)
  i = 1
  while(i<=(nrow(dat)-1)){
    # go over every pair of screen event
    curStatus <- messages[[i]]$data$status
    nextStatus <- messages[[i+1]]$data$status
    # if it is not a "ON"-"OFF" pair, shift the pointer by one
    if(curStatus!="ON" || nextStatus !="OFF"){
      i = i+1
      next
    }
    on <- as.POSIXct(dat[i,]$dt_record, tz="")
    off <- as.POSIXct(dat[i+1,]$dt_record, tz="")
    if(off - on < as.difftime(3, units="hours"))
      events <- rbind(events, data.frame(on=format(on,usetz=TRUE), 
                                         off=format(off,usetz=TRUE)))
    
    i = i+2
 }
 return(events)
}
systemsensAPI.activityLog <- function(imei, begin=NULL, end=NULL){
  dat <- systemsensAPI.query(imei, "activitylog", begin, end)  
  if(is.null(dat))
    return(NULL)
  
  messages <- systemsensAPI.jsonStringsToList(dat$message)
  activity_dat <- ldply(messages, function(msg){
    logs <-msg$data
    ldply(logs, function(log){
      if(!is.null(log$activity)){
        activity <- log$activity
        # convert the timestamp to posix object (timestamp is the number of nano second since 1970-1-1)
        time_stamp <-as.POSIXct.numeric(log$time_stamp/1000000000.0, origin="1970-1-1", tz="GMT")
        time_stamp <- format(time_stamp , tz="",usetz=TRUE)
        event <-log$event
        data.frame(activity=activity,
                   time_stamp=time_stamp,
                   event=event)
      }
    })
  })
  activity_dat$.id <- NULL
  splitted_dat <- ldply(str_split(activity_dat$activity, '/'), function(r){
    data.frame(appname=r[1], activityname=r[2])
  })
  return(cbind(activity_dat,splitted_dat ))
}

# systemsensAPI.networkUsage: return the cumulative network usage before each time phone restarts.
# Return value example:
#      Tx        Rx          time_stamp
#    2552369   3554820 2013-04-16 17:00:01
#    534390    1150861 2013-04-21 21:46:19

systemsensAPI.totalNetworkUsage<- function(imei, begin=NULL, end=NULL){
  dat <- systemsensAPI.query(imei, "netiflog", begin, end)  
  if(is.null(dat))
    return(NULL)
  messages <- systemsensAPI.jsonStringsToList(dat$message)
  raw_netlog <- ldply(messages, function(msg){
    data.frame(Tx=msg$data[["TotalTxBytes"]], Rx=msg$data[["TotalRxBytes"]], MobileTx=msg$data[["MobileTxBytes"]] , MobileRx=msg$data[["MobileRxBytes"]])
  })
  raw_netlog$time_stamp <- as.POSIXct(dat$dt_record)
  # get the record before the gap, which should be the cumulative RX,TX amount before phone restarts
  netlog <- raw_netlog[head(raw_netlog$MobileTx, -1) - tail(raw_netlog$MobileTx, -1) > 5000 &
                         head(raw_netlog$MobileRx, -1) - tail(raw_netlog$MobileRx, -1) > 5000 &
                       head(raw_netlog$Tx, -1) - tail(raw_netlog$Tx, -1) > 5000 &
                         head(raw_netlog$Rx, -1) - tail(raw_netlog$Rx, -1) > 5000 &
                         head(raw_netlog$time_stamp , -1) - tail(raw_netlog$time_stamp , -1) < -as.difftime(5, unit="mins")
                         ,]
  netlog <- rbind(netlog, tail(raw_netlog,1))
  netlog
}

