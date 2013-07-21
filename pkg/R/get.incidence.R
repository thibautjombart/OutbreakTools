

## ###################
## ## get.incidence ##
## ###################

## setMethod("get.incidence", "obkData", function(x, use=c("collection"), ...){
##     if(get.nrecords(x)<1) return(NULL)

##     ## get dates
##     dates <- get.dates(x, data="records")
##     first.date <- min(dates <- get.dates(x, data="records"))
##     last.date <- max(dates <- get.dates(x, data="records"))
##     out.dates <- seq(first.date, last.date, by=1) # output dates

##     ## get vector of "positive outcomes" ##

##     ## split vector per individual ##

##     ## get date of first positive for each individual ##

##     ## ## auxiliary function: find 1st positive for one individual
##     ## f1 <- function(e){
##     ##     ## no positive outcome
##     ##     if() return(NULL)

##     ##     ## at least one positive outcome
##     ##     ## find first date
##     ##     ## return
##     ##     return(out)
##     ## }

##     ## fill in the output ##


## }) # end get.incidence

