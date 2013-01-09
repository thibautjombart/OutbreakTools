
############################
####  CLASSE DEFINITION ####
############################

## CLASS DESCRIPTION:
## - instance of obkSequences store DNA sequences for a given sample
## - sequences are stored as a (possibly named) list
## - each element of the list corresponds to a locus
## - names of the list are locus names
## - each element of the list is a DNAbin matrix
## (i.e., if there are several sequences, they are to be aligned)

setClass("obkSequences", representation(dna="listOrNULL"), prototype(dna=NULL))







######################
####  CONSTRUCTOR ####
######################

## INPUT DESCRIPTION:
## dna: a list of DNA sequences
## locus: a vector of characters indicating which locus each sequence corresponds to
setMethod("initialize", "obkSequences", function(.Object, dna=NULL, locus=NULL) {

    ## RETRIEVE PROTOTYPED OBJECT ##
    x <- .Object

    ## escape if no info provided ##
    if(is.null(dna)) return(x)


    ## PROCESS ARGUMENTS ##
    ## convert matrices of characters into DNAbin ##
    if(is.matrix(dna) && is.character(dna)) dna <- as.DNAbin(dna)

    ## force list type for DNAbin matrices ##
    if(is.matrix(dna) && inherits(dna, "DNAbin")) dna <- as.list(dna)

    ## convert list of characters to DNAbin list ##
    if(is.list(dna) && all(sapply(dna, is.character))) dna <- lapply(dna, as.DNAbin)

    ## check that dna is now a DNAbin list ##
    if(!is.list(dna) || !inherits(dna, "DNAbin")) stop("dna input could not be processed into a DNAbin list")


    ## SHAPE OUTPUT ##
    ## no locus info => unnamed list of length 1
    if(is.null(locus)){
        x@dna <- list(as.matrix(dna))
        return(x)
    }

    ## otherwise: locus info provided ##
    ## check length consistency
    if(length(dna) != length(locus)) stop(paste("Length mismatch (dna:", length(dna), "items; locus:", length(locus),"items)"))

    x@dna <- lapply(unique(locus), function(loc) as.matrix(dna[locus==loc]))
    names(x@dna) <- unique(locus)

    return(x)
}) # end obkSequences constructor



## CODE FOR BASIC TEST
## library(ape)








####################
####  ACCESSORS ####
####################


#### GENERAL ACCESSORS ####

## accessor: nPatients
setGeneric("nPatients", function(x, ...) standardGeneric("nPatients"))

setMethod("nPatients","outbreak", function(x,...){
    return(x@n.patients)
})


## accessor: nSwabs
setGeneric("nSwabs", function(x, ...) standardGeneric("nSwabs"))

setMethod("nSwabs","outbreak", function(x,...){
    return(x@n.swabs)
})


## accessor: nDna
setGeneric("nDna", function(x, ...) standardGeneric("nDna"))

setMethod("nDna","outbreak", function(x,...){
    return(x@n.dna)
})


## accessor: patients
setGeneric("patients", function(x, ...) standardGeneric("patients"))

setMethod("patients","outbreak", function(x, what=c("id","labels","swabs","dna"), ...){
    what <- match.arg(what)
    if(what %in% c("id","labels")) return(x@patient.id)
    if(what=="swabs") return(x@swab.patients)
    if(what=="dna") return(x@dna.patients)
})


## accessor: dates
setGeneric("dates", function(x, ...) standardGeneric("dates"))

setMethod("dates","outbreak", function(x, what=c("swabs","dna"), ...){
    what <- match.arg(what)
    if(what=="swabs") return(x@swab.dates)
    if(what=="dna") return(x@dna.dates)
})


## accessor: days
setGeneric("days", function(x, ...) standardGeneric("days"))

setMethod("days","outbreak", function(x, what=c("swabs","dna"), ...){
    what <- match.arg(what)
    if(what=="swabs") return(x@swab.days)
    if(what=="dna") return(x@dna.days)
})


## accessor: swabs
setGeneric("swabs", function(x, ...) standardGeneric("swabs"))

setMethod("swabs","outbreak", function(x, what=c("matrix", "data", "patients","types", "dates", "days"), ...){
    what <- match.arg(what)

    if(what=="matrix") return(x@swab.mat)
    if(what=="data") return(x@swabs)
    if(what=="patients") return(x@swab.patients)
    if(what=="types") return(x@swab.types)
    if(what=="dates") return(x@swab.dates)
    if(what=="days") return(x@swab.days)
})


## accessor: dna
setGeneric("dna", function(x, ...) standardGeneric("dna"))

setMethod("dna","outbreak", function(x, what=c("data", "patients", "types", "dates", "days"), ...){
    what <- match.arg(what)

    if(what=="data") return(x@dna)
    if(what=="patients") return(x@dna.patients)
    if(what=="types") return(x@dna.types)
    if(what=="dates") return(x@dna.dates)
    if(what=="days") return(x@dna.days)
})




#### SPECIFIC ACCESSORS: SWABS ####

## accessor: swabLabels
setGeneric("swabLabels", function(x, ...) standardGeneric("swabLabels"))

setMethod("swabLabels","outbreak", function(x, ...){
    out <- paste(swabs(x,"patients"), swabs(x,"types"),swabs(x,"dates"),sep="/")
    return(out)
})



## accessor: swabPatients
setGeneric("swabPatients", function(x) standardGeneric("swabPatients"))

setMethod("swabPatients","outbreak", function(x){
    return(swabs(x, what="patients"))
})


## accessor: swabDates
setGeneric("swabDates", function(x, value) standardGeneric("swabDates"))

setMethod("swabDates","outbreak", function(x){
    return(swabs(x, what="dates"))
})


## accessor: swabDays
setGeneric("swabDays", function(x, value) standardGeneric("swabDays"))

setMethod("swabDays","outbreak", function(x){
    return(swabs(x, what="days"))
})




#### SPECIFIC ACCESSORS: DNA ####

## accessor: dnaLabels
setGeneric("dnaLabels", function(x, value) standardGeneric("dnaLabels"))

setMethod("dnaLabels","outbreak", function(x){
    return(labels(x@dna))
})


## accessor: dnaPatients
setGeneric("dnaPatients", function(x, value) standardGeneric("dnaPatients"))

setMethod("dnaPatients","outbreak", function(x){
    return(dna(x, what="patients"))
})


## accessor: dnaDates
setGeneric("dnaDates", function(x, value) standardGeneric("dnaDates"))

setMethod("dnaDates","outbreak", function(x){
    return(dna(x, what="dates"))
})


## accessor: dnaDays
setGeneric("dnaDays", function(x, value) standardGeneric("dnaDays"))

setMethod("dnaDays","outbreak", function(x){
    return(dna(x, what="days"))
})












############# OLD STUFF: INCLUDE (ABANDONNED) REPLACEMENT METHODS ###########
## #### SPECIFIC ACCESSORS: SWABS ####

## ## accessor: swabLabels
## setGeneric("swabLabels", function(x, ...) standardGeneric("swabLabels"))

## setMethod("swabLabels","outbreak", function(x, ...){
##     out <- paste(swabs(x,"patients"), swabs(x,"types"),swabs(x,"dates"),sep="/")
##     return(out)
## })



## ## accessor: swabPatients
## setGeneric("swabPatients", function(x) standardGeneric("swabPatients"))
## setGeneric("swabPatients<-", function(x, value) standardGeneric("swabPatients<-"))

## setMethod("swabPatients","outbreak", function(x){
##     return(swabs(x, what="patients"))
## })

## setReplaceMethod("swabPatients","outbreak",function(x,value) {
##     ## if no swab
##     if(is.null(swabs(x))){
##         warning("object has no swab information stored.")
##         return(x)
##     }

##     ## otherwise, test length of replacement if there was a (non-NULL) value before
##     if(!is.null(swabPatients(x)) && !is.null(value)){
##         if(length(value)!=length(swabPatients(x))) stop(paste("Wrong length provided for replacement (old:",length(swabPatients(x)), ", new:",length(value),")"))
##     }

##     ## accept if this is OK
##     slot(x,"swab.patients",check=TRUE) <- as.character(value)

##     ## return
##     return(x)
## })



## ## accessor: swabDates
## setGeneric("swabDates", function(x, value) standardGeneric("swabDates"))
## setGeneric("swabDates<-", function(x, value) standardGeneric("swabDates<-"))

## setMethod("swabDates","outbreak", function(x){
##     return(swabs(x, what="dates"))
## })

## setReplaceMethod("swabDates","outbreak",function(x,value) {
##     ## if no swab
##     if(is.null(swabs(x))){
##         warning("object has no swab information stored.")
##         return(x)
##     }

##     ## otherwise, test length of replacement if there was a (non-NULL) value before
##     if(!is.null(swabDates(x)) && !is.null(value)){
##         if(length(value)!=length(swabDates(x))) stop(paste("Wrong length provided for replacement (old:",length(swabDates(x)), ", new:",length(value),")"))
##     }

##     ## handle NULL argument
##     if(is.null(value)){
##         slot(x,"swab.dates",check=TRUE) <- NULL
##         return(x)
##     }
##     ## convert characters to POSIXct
##     if(is.character(value)){
##         value <- as.POSIXct(value)
##     }
##     if(inherits(value, "POSIXct")){
##         slot(x,"swab.dates",check=TRUE) <- value
##     } else {
##         stop("Unknown type for replacement; accepted inputs for dates are: characters (yyyy-mm-dd) or POSIXct")
##     }

##     ## return
##     return(x)
## })


## ## accessor: swabDays
## setGeneric("swabDays", function(x, value) standardGeneric("swabDays"))
## setGeneric("swabDays<-", function(x, value) standardGeneric("swabDays<-"))

## setMethod("swabDays","outbreak", function(x){
##     return(swabs(x, what="days"))
## })

## setReplaceMethod("swabDays","outbreak",function(x,value) {
##     ## if no swab
##     if(is.null(swabs(x))){
##         warning("object has no swab information stored.")
##         return(x)
##     }

##     ## otherwise, test length of replacement if there was a (non-NULL) value before
##     if(!is.null(swabDays(x)) && !is.null(value)){
##         if(length(value)!=length(swabDays(x))) stop(paste("Wrong length provided for replacement (old:",length(swabDays(x)), ", new:",length(value),")"))
##     }

##     ## if this is OK, check type
##     if(inherits(value, c("integer","numeric"))){
##         slot(x,"swab.days",check=TRUE) <- value
##     } else {
##         stop("Unknown type for replacement; accepted classes for days are: integer or numeric")
##     }

##     ## return
##     return(x)
## })





## #### SPECIFIC ACCESSORS: DNA ####

## ## accessor: dnaLabels
## setGeneric("dnaLabels", function(x, value) standardGeneric("dnaLabels"))
## setGeneric("dnaLabels<-", function(x, value) standardGeneric("dnaLabels<-"))

## setMethod("dnaLabels","outbreak", function(x){
##     return(labels(x@dna))
## })

## setReplaceMethod("dnaLabels","outbreak",function(x,value) {
##     ## if no DNA
##     if(is.null(dna(x))){
##         warning("object has no DNA information stored.")
##         return(x)
##     }

##     ## otherwise
##     if(!is.null(value) && nrow(dna(x))!=length(value)){
##         stop(paste("Wrong length provided for replacement (old:", nrow(dna(x)), ", new:", length(value),")"))
##     }
##     rownames(x@dna) <- as.character(value)

##     ## return
##     return(x)
## })


## ## accessor: dnaPatients
## setGeneric("dnaPatients", function(x, value) standardGeneric("dnaPatients"))
## setGeneric("dnaPatients<-", function(x, value) standardGeneric("dnaPatients<-"))

## setMethod("dnaPatients","outbreak", function(x){
##     return(dna(x, what="patients"))
## })

## setReplaceMethod("dnaPatients","outbreak",function(x,value) {
##     ## if no DNA
##     if(is.null(dna(x))){
##         warning("object has no DNA information stored.")
##         return(x)
##     }

##     ## otherwise, test length of replacement if there was a (non-NULL) value before
##     if(!is.null(dnaPatients(x)) && !is.null(value)){
##         if(length(value)!=length(dnaPatients(x))) stop(paste("Wrong length provided for replacement (old:",length(dnaPatients(x)), ", new:",length(value),")"))
##     }

##     ## accept if this is OK
##     slot(x,"dna.patients",check=TRUE) <- as.character(value)

##     ## return
##     return(x)
## })



## ## accessor: dnaDates
## setGeneric("dnaDates", function(x, value) standardGeneric("dnaDates"))
## setGeneric("dnaDates<-", function(x, value) standardGeneric("dnaDates<-"))

## setMethod("dnaDates","outbreak", function(x){
##     return(dna(x, what="dates"))
## })

## setReplaceMethod("dnaDates","outbreak",function(x,value) {
##     ## if no DNA
##     if(is.null(dna(x))){
##         warning("object has no DNA information stored.")
##         return(x)
##     }

##     ## otherwise, test length of replacement if there was a (non-NULL) value before
##     if(!is.null(dnaDates(x)) && !is.null(value)){
##         if(length(value)!=length(dnaDates(x))) stop(paste("Wrong length provided for replacement (old:",length(dnaDates(x)), ", new:",length(value),")"))
##     }

##     ## handle NULL argument
##     if(is.null(value)){
##         slot(x,"dna.dates",check=TRUE) <- NULL
##         return(x)
##     }

##     ## convert characters to POSIXct
##     if(is.character(value)){
##         value <- as.POSIXct(value)
##     }

##     if(inherits(value, "POSIXct")){
##         slot(x,"dna.dates",check=TRUE) <- value
##     } else {
##         stop("Unknown type for replacement; accepted classes for dates are: POSIXct, integer or numeric")
##     }

##     ## return
##     return(x)
## })


## ## accessor: dnaDays
## setGeneric("dnaDays", function(x, value) standardGeneric("dnaDays"))
## setGeneric("dnaDays<-", function(x, value) standardGeneric("dnaDays<-"))

## setMethod("dnaDays","outbreak", function(x){
##     return(dna(x, what="days"))
## })

## setReplaceMethod("dnaDays","outbreak",function(x,value) {
##     ## if no DNA
##     if(is.null(dna(x))){
##         warning("object has no DNA information stored.")
##         return(x)
##     }

##     ## otherwise, test length of replacement if there was a (non-NULL) value before
##     if(!is.null(dnaDays(x)) && !is.null(value)){
##         if(length(value)!=length(dnaDays(x))) stop(paste("Wrong length provided for replacement (old:",length(dnaDays(x)), ", new:",length(value),")"))
##     }

##     ## if this is OK, check type
##     if(inherits(value, c("integer","numeric"))){
##         slot(x,"dna.days",check=TRUE) <- value
##     } else {
##         stop("Unknown type for replacement; accepted classes for days are: integer or numeric")
##     }

##     ## return
##     return(x)
## })



































## model:
## setGeneric("foo", function(x, ...) standardGeneric("foo"))
## setGeneric("foo<-", function(x, value) standardGeneric("foo<-"))
##
##setMethod("foo","outbreak", function(x,...){
##    return(x@foo)
## })
##
##
## setReplaceMethod("foo","outbreak",function(x,value) {
##     if(is.null(value)){
##         slot(x, "foo", check=TRUE) <- value
##         return(x)
##     }
##    ## tests on value
##    ## ...
##    slot(x,"foo",check=TRUE) <- value[1]
##    return(x)
## })




########################
####  BASIC METHODS ####
########################

## setMethod ("show", "outbreak", function(object){
## })




