require(cpm)
getChangePoints <- function(user, feature, wdays, ARL0=300, startup=20){
  if(feature$type == "categorical")
    stop("change detection for categorical data is currently unsupported")

  # detect change in the slected variable
  dat <- project$dat
  #dat <- dat[dat$user.id==user$id & dat$dayType %in% dayType,]
  dat <- dat[dat$user.id==user$id & wday(dat$date) %in% wdays,]
  dat <- dat[!is.na(dat[[feature$name]]),]
  series <- dat[[feature$name]]
  
  if(nrow(dat)> startup){

    # check if it is binary data
    if(feature$type=="logical"){
      ret <- processStream(series, "FET",ARL0=ARL0,startup=startup,lambda=0.3)
    }
    else{
      ret <- processStream(series, cpmType="Mann-Whitney", ARL0=ARL0, startup=startup)
    }
    
    return(data.frame(changePoint=dat$date[ret$changePoints], 
                      detectionTime=dat$date[detectionTimes]))
  }else{
    return(NULL)
  }
}