#Install the libraries
#install.packages("purrr")
library(httr)
library(data.table)
library(dplyr)
library(xml2)
library(purrr)
source("functions.r")
library('keboola.r.docker.application')

##Returns the time intervals for the API call
getIntervals <- function(time.frame) {
  
  if (time.frame == "year")  {
    res <-data.frame(start.date = Sys.Date() - 31, end.date = Sys.Date())
    for(i in 2:12) {  a <-
      data.frame(
        start.date = (Sys.Date() - (31 * i)),
        end.date = (Sys.Date() - (31 * (i - 1)))-1)
        res<-rbind(res, a)  }
    return(res)
  } else if (time.frame == "month")  {
    res <-data.frame(start.date = Sys.Date() - 31, end.date = Sys.Date())
  } else
    stop("Invalid Time Frame issued - valid options: month, year")
  
}

##Call The CJ API 

callAPI<-function(intervals,apikey,date.type){
  
  endpoint<-"https://commission-detail.api.cj.com/v3/commissions"
  
  data<-data.frame()
  
  for(i in 1:dim(intervals)[1]){
  updatedargs<-list("start-date"=intervals$start.date[i],"end-date"=intervals$end.date[i],"date-type"=date.type)
  
  r <- RETRY("GET",endpoint, times=3, query=updatedargs, add_headers(authorization = apikey))
  
  results<-if(r$status_code==200) {
    res<-xml2::as_list(content(r,"parsed",encoding = "UTF-8")) 
    names<-names(res$commissions[[1]])
    res2<-t(rbindlist(res))
    res3<-mutate_all(as.data.frame(res2),unlist)
    names(res3)<-names
    res3
  }else {
    stop(paste("API Call n.",call,"failed. status:",content(r)$status, sep=" "),call.=TRUE)}
  
  data<-bind_rows(data,results)
  
  #Sys.sleep(3)
  
  }
  
}