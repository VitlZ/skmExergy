# "getSupplyCurve" 

#' Query SKM Exergy API.
#'
#' This function load data about supply curves from SKM Exergy (\url{https://exergy.skmenergy.com/}) into a data.frame and plot it. You must be a SKM MP client for access to Exergy data.
#'
#' @param series_names Character vector with names of series. Example: c("CNPPLAN_RU1_H","PROPLAN_RU1_H","SPOT_RU1_H")
#' @param fromDate Start date as character (YYYY-MM-DD) or as date in R format. Examples:"2014-21-08" or "as.Date(Sys.time())"  
#' @param toDate End date as character (YYYY-MM-DD) or as date in R format. Examples:"2014-22-08" or "as.Date(Sys.time())+1"  
#' @param fromHour Start hour as character ("HH"). "00" by default for first hour of day.
#' @param toHour End hour as character ("HH"). "23" by default for the last hour of day.
#' @return Dataframe with data for series and time period.
#' @examples 
#' \dontrun{
#' # Get data for the last week
#' today<-as.Date(Sys.time())
#' seriesList<-c("CNPPLAN_RU1_H","PROPLAN_RU1_H","SPOT_RU1_H")
#'
#' dataSet<-getData(seriesList,today-6,today)
#' dataSet}
#' @export getSC

# Current bid curve analysis

getSC<-function(hour,date=as.Date(Sys.time()),prizeZone=1,plotCurves=TRUE){
  
  # Test input
  stopifnot(!is.null(hour))
  
  # Get data
  if(prizeZone==1){prizeZoneStr<-"RU1"}else{prizeZoneStr<-"RU2"}
  if(nchar(as.character(hour))==1){hourStr<-paste(0,as.character(hour),sep="")}else{hourStr<-as.character(hour)}
  yesterday<-date-1
  weekAgo<-date-7
  
  ds<-data.frame(matrix(NA,300,9))
  names(ds)<-c("bidToday","bidVolumeToday","bidCumVolumeToday","bidYesterday","bibVolumeYesterday","bidCumVolumeYesterday","bidWeekAgo","bidVolumeWeekAgo","bidCumVolumeWeekAgo")
  
  for(bid in 1:300){
    
    bidStr<-bid
    if(bid<10){bidStr<-paste("00",bid,sep="")}
    if(bid>9 & bid<100){bidStr<-paste("0",bid,sep="")}
    
  hourDsToday<-getData(c(paste("SUPPLYPRICE",bidStr,"_",prizeZoneStr,"_H",sep=""),paste("SUPPLYVOL",bidStr,"_",prizeZoneStr,"_H",sep="")),date,date,fromHour=hourStr,toHour=hourStr)
  ds[bid,1]<-hourDsToday[1,2]
  ds[bid,2]<- hourDsToday[1,3]
  
  hourDsYesterday<-getData(c(paste("SUPPLYPRICE",bidStr,"_",prizeZoneStr,"_H",sep=""),paste("SUPPLYVOL",bidStr,"_",prizeZoneStr,"_H",sep="")),yesterday,yesterday,fromHour=hourStr,toHour=hourStr)
  ds[bid,4]<-hourDsYesterday[1,2]
  ds[bid,5]<- hourDsYesterday[1,3]

  hourDsWeekAgo<-getData(c(paste("SUPPLYPRICE",bidStr,"_",prizeZoneStr,"_H",sep=""),paste("SUPPLYVOL",bidStr,"_",prizeZoneStr,"_H",sep="")),weekAgo,weekAgo,fromHour=hourStr,toHour=hourStr)
  ds[bid,7]<-hourDsWeekAgo[1,2]
  ds[bid,8]<-hourDsWeekAgo[1,3]
  
  }
 
  # Add cumulative sum
  ds[,3]<-cumsum(ds[,2])
  ds[,6]<-cumsum(ds[,5])
  ds[,9]<-cumsum(ds[,8])

  # Plot
  if(plotCurves==TRUE){
    
    par(mfrow=c(2,1)) 
    
    plot(ds[2:300,1]~ds[2:300,3],xlim=c(min(ds[,3],na.rm=TRUE)-5000,max(ds[,3],na.rm=TRUE)+5000),type="l",col=1,main=paste(date,"Hour",hour),lwd=2,xlab="MW",ylab="RUR")
    lines(ds[2:300,4]~ds[2:300,6],col=4)
    lines(ds[2:300,7]~ds[2:300,9],col=2)
    #legend("topleft", pch = 20, col = c(1, 4, 2), legend = c("Today","Yesterday", "Week ago")) 
    abline(h=1000,col=8,lty=3) 
    
    plot(ds[2:300,1]~cumsum(ds[2:300,2]),xlim=c(min(ds[,2],na.rm=TRUE)-5000,max(cumsum(ds[2:300,2]),na.rm=TRUE)+5000),type="l",col=1,main=paste("Only price part",date,"Hour",hour),lwd=2,xlab="MW",ylab="RUR")
    lines(ds[2:300,4]~cumsum(ds[2:300,5]),col=4)
    lines(ds[2:300,7]~cumsum(ds[2:300,8]),col=2)
    #legend("topleft", pch = 20, col = c(1, 4, 2), legend = c("Today","Yesterday", "Week ago")) 
    abline(h=1000,col=8,lty=3) 
    
  }

  return(ds)

}
