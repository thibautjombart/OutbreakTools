

###################
## get.incidence ##
###################
setGeneric("get.incidence", function(x, ...) standardGeneric("get.incidence"))



#################
## Date method ##
#################
setMethod("get.incidence", "Date", function(x, first.date=NULL, last.date=NULL, interval=1, ...){
    ## GET DATES OF THE OUTPUT ##
    if(is.null(first.date)){
        first.date <- min(x, na.rm=TRUE)
    } else {
        first.date <- min(min(x, na.rm=TRUE), first.date)
    }
    if(is.null(last.date)){
        last.date <- max(x, na.rm=TRUE)
    } else {
        last.date <- max(max(x, na.rm=TRUE), last.date)
    }
    out.dates <- seq(first.date, last.date, by=interval) # output dates


    ## COMPUTE INCIDENCE ##
    ## incid is computed for time intervals
    ## incid on interval [d1,d2[ is named after d1
    breaks <- c(as.integer(out.dates), as.integer(last.date)+interval)
    incid <- table(cut(as.integer(x), breaks=breaks, right=FALSE))
    out <- data.frame(date=out.dates, incidence=as.integer(incid))
    return(out)
}) # end Date method





#########################
## obkSequences method ##
#########################
setMethod("get.incidence", "obkSequences", function(x, first.date=NULL, last.date=NULL, ...){
    if(is.null(x) || get.nsequences(x)<1) return(NULL)

    out <- get.incidence(x@meta$date)
    return(out)

}) # end obkSequences method






########################
## obkContacts method ##
########################
setMethod("get.incidence", "obkContacts", function(x, first.date=NULL, last.date=NULL, ...){
    if(is.null(x) || get.ncontacts(x)<1 || !is.networkDynamic(x@contacts)) return(NULL)

    ## CHECK THAT THIS IS A DYNAMIC CONTACT NETWORK ##
    out <- get.incidence(as.data.frame(x)$onset)
    return(out)

}) # end obkContacts method




## ####################
## ## obkData method ##
## ####################
## setMethod("get.incidence", "obkData", function(x, use=c("collection"), ...){
##     if(get.nrecords(x)<1) return(NULL)

## }) # end get.incidence

