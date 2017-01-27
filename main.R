
#=======BASIC INFO ABOUT THE CJ EXTRACTOR========#

##This file serves as the extractor for Commissions from CJ.com
#source("devel.r")
source("functions.r")

#=======CONFIGURATION========#
## initialize application
app <- DockerApplication$new('/data/')

app$readConfig()

## access the supplied value of 'myParameter'
authorization<-app$getParameters()$`#authorization`
date.type<-app$getParameters()$datetype
time.frame<-app$getParameters()$timeframe

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


