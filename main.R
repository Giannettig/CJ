#Install the libraries
#install.packages("purrr")
library(httr)
library(data.table)
library(dplyr)
library(xml2)
#library(purrr)

#=======BASIC INFO ABOUT THE CJ EXTRACTOR========#

##This file serves as the extractor for Commissions from CJ.com
#source("devel.r")

#=======CONFIGURATION========#
## initialize application
library('keboola.r.docker.application')
app <- DockerApplication$new('/data/')

app$readConfig()

## access the supplied value of 'myParameter'
authorization<-app$getParameters()$`#authorization`
date.type<-app$getParameters()$datetype
time.frame<-app$getParameters()$timeframe

##Catch config errors

if(is.null(authorization)) stop("please enter you developer  key in the #Authorization config field")
if(is.null(date.type)) stop("datetype config field is missing")
if(is.null(time.frame)) stop("timeframe config missing")

###Functions

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
      res2<-as.data.frame(t(rbindlist(res)))
      rownames(res2) <- c()
      names(res2)<-names
      for(i in 1:dim(res2)[1]){
         
        for(j in 1:dim(res2)[2]){
          res2[i,j]<-ifelse(is.null(unlist(res2[i,j])),"NA",unlist(res2[i,j]))
        }
          
      }
      res2
    }else {
      stop(paste("API Call n.",call,"failed. status:",content(r)$status, sep=" "),call.=TRUE)}
    
    data<-bind_rows(data,results)
    
    #Sys.sleep(3)
    
  }
  
}

args<-date.type

intervals<-getIntervals(time.frame)
res<-try(callAPI(intervals, authorization, args))


###Export the data

write.csv(as.matrix(res),"out/tables/cj-results.csv", row.names=FALSE)

# authorization : your API KEY
# date-type : event|posting
# start-date:   Use this parameter to specify the first date included in your query (1-day minimum, 31 days maximum).
# end-date: Use this parameter to specify the last date included in your query (1-day minimum, 31 days maximum).
# cids: Use this parameter to specify the CID of the joined advertiser or publisher
# action-types : Use this parameter to specify a specific type of action. Options include the following:
#       bonus
#       click
#       impression
#       sale
#       lead
#       advanced sale
#       advanced lead
#       performance incentive
# aids : Use this parameter to specify IDs for specifics Ad IDs.
# action-status:Use this parameter to specify actions of a particular status. Options include the following.
#       new
#       locked
#       extended
#       closed
# commission-id :Use this parameter to specify the Commission ID. 
# website-ids : Use this parameter to specify a particular Web site or set of Web sites.


