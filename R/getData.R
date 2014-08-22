# "getData" as the main function

#' Query SKM Exergy API.
#'
#' This function creates a web query url and reads data from SKM Exergy (\url{https://exergy.skmenergy.com/}) into a data.frame. You must be a client for use it.
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
#' @export getData
getData<-function(series_names,fromDate,toDate,fromHour="00",toHour="23"){
  
  # Test input
  stopifnot(is.character(series_names), !is.null(series_names))
  stopifnot(!is.null(fromDate))
  stopifnot(!is.null(toDate))
  stopifnot(is.character(fromHour), !is.null(fromHour))
  stopifnot(is.character(toHour), !is.null(toHour))
     
  # Make series string 
  series_string<-character()
  for(s in 1:length(series_names)){
    series_string<-paste(series_string,series_names[s],sep=",")
  }
  series_string<-sub(",",series_string,replacement="") # Delete first comma
  
  # Get data
  ds<-read.csv(paste("https://exergy.skmenergy.com/api/data/?alt=csv&from=",fromDate,"T",fromHour,":00&to=",toDate,"T",toHour,":00&series=",series_string,sep=""))
  return(ds)
}


