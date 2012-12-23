
#################################
####  DEFINITIONS OF CLASSES ####
#################################

##################
## outbreak class
##################
setOldClass("DNAbin")
setOldClass("POSIXct")
setClassUnion("matrixOrNULL", c("matrix","NULL"))
setClassUnion("DNAbinOrNULL", c("DNAbin","NULL"))
setClassUnion("date", c("POSIXct","numeric","integer","NULL"))

## class description:
## @n: number of cases
## @case.id: identifier of the cases
## @swabs: matrix indicating positive/negative swabs, with patient/swab type in rows, dates in columns
## @swab.case: indicator of the case (i.e., patient) for each row of '@swabs'; values must have a match in '@case.id'
## @swab.type: indicator of the type of swab for each row of '@swabs'
## @swab.date: sequence of dates from first to last swab, by steps of 1 day; format depends on input, may be POSIXct
## @swab.day: sequence of integer dates from first to last swab, by steps of 1 day; '0' is the first swab or sequence collected
## @dna: DNA sequences sampled for some or all cases, as a DNAbin object
## @dna.case: indicator of the case (i.e., patient) for each row of '@dna'; values must have a match in '@case.id'
## @dna.date: collection date of each DNA sequence in '@dna'; format depends on input, may be POSIXct
## @dna.day: integer collection date of each DNA sequence in '@dna'; '0' is the first swab or sequence collected

setClass("outbreak", representation(n="integer", case.id="character",
                                    swabs="matrixOrNULL", swab.case="character", swab.type="character", swab.date="date", swab.day="integer",
                                    dna="DNAbinOrNULL", dna.case="character", dna.date="date", dna.day="integer"),
         prototype(n=0L, case.id=character(0),
                   swabs=NULL, swab.case=character(0), swab.type=character(0), swab.date=NULL, swab.day=integer(0),
                   dna=NULL, dna.case=character(0), dna.date=NULL, dna.day=integer(0)))







#######################
####  CONSTRUCTORS ####
#######################

########################
## outbreak constructor
########################
setMethod("initialize", "outbreak", function(.Object, ...) {
    x <- .Object
    input <- list(...)
    ## if(length(input)==1) names(input) <- "snp"
    ## if(length(input)>1 && ! "snp" %in% names(input)) names(input)[1] <- "snp"

    ## handle arguments ##
    ## ...
    return(x)
}) # end SNPbin constructor









####################
####  ACCESSORS ####
####################

## accessor: nCases
setGeneric("nCases", function(x, ...) standardGeneric("nCases"))
setMethod("nCases","outbreak", function(x,...){
    return(x@n)
})


## accessor: cases
setGeneric("cases", function(x, ...) standardGeneric("cases"))

setMethod("cases","outbreak", function(x, what=c("id","labels","swabs","dna"), ...){
    what <- match.arg(what)
    if(what %in% c("id","labels")) return(x@case.id)
    if(what=="swabs") return(x@swab.case)
    if(what=="dna") return(x@dna.case)
})


## accessor: dates
setGeneric("dates", function(x, ...) standardGeneric("dates"))

setMethod("dates","outbreak", function(x, what=c("swabs","dna"), ...){
    what <- match.arg(what)
    if(what=="swabs") return(x@swab.date)
    if(what=="dna") return(x@dna.date)
})


## accessor: days
setGeneric("days", function(x, ...) standardGeneric("days"))

setMethod("days","outbreak", function(x, what=c("swabs","dna"), ...){
    what <- match.arg(what)
    if(what=="swabs") return(x@swab.day)
    if(what=="dna") return(x@dna.day)
})


## accessor: swab
setGeneric("swab", function(x, ...) standardGeneric("swab"))
setGeneric("swab<-", function(x, value) standardGeneric("swab<-"))

setMethod("swab","outbreak", function(x, what=c("data", "cases", "dates", "days"), ...){
    what <- match.arg(what)

    if(what=="data") return(x@swab)
    if(what=="cases") return(x@swab.case)
    if(what=="dates") return(x@swab.date)
    if(what=="day") return(x@swab.day)
})


setReplaceMethod("swab","outbreak",function(x,value) {
    ## if NULL provided
    if(is.null(value)){
        slot(x, "swab", check=TRUE) <- value
        return(x)
    }

    ## if matrix provided
    if(inherits(value,"SWABbin")){
        slot(x,"swab",check=TRUE) <- value[1]
    }

    ## return
    return(x)
})


## accessor: dna
setGeneric("dna", function(x, ...) standardGeneric("dna"))
setGeneric("dna<-", function(x, value) standardGeneric("dna<-"))

setMethod("dna","outbreak", function(x, what=c("data", "cases", "dates", "days"), ...){
    what <- match.arg(what)

    if(what=="data") return(x@dna)
    if(what=="cases") return(x@dna.case)
    if(what=="dates") return(x@dna.date)
    if(what=="day") return(x@dna.day)
})


setReplaceMethod("dna","outbreak",function(x,value) {
    ## if NULL provided
    if(is.null(value)){
        slot(x, "dna", check=TRUE) <- value
        return(x)
    }

    ## if DNAbin matrix provided
    if(inherits(value,"DNAbin")){
        slot(x,"dna",check=TRUE) <- value[1]
    }

    ## if character matrix provided
    if(is.character(value) & is.matrix(value)){
        slot(x,"dna",check=TRUE) <- as.DNAbin(value[1])
    }

    ## return
    return(x)
})


## accessor: dnaLabels
setGeneric("dnaLabels", function(x, value) standardGeneric("dnaLabels"))
setGeneric("dnaLabels<-", function(x, value) standardGeneric("dnaLabels<-"))

setMethod("dnaLabels","outbreak", function(x, ...){
    return(labels(x@dna))
})

setReplaceMethod("dnaLabels","outbreak",function(x,value) {
    ## if no DNA
    if(is.null(dna(x))){
        return(NULL)
    }

    ## otherwise
    rownames(x@dna) <- as.character(value)

    ## return
    return(x)
})


## accessor: dnaCases
setGeneric("dnaCases", function(x, value) standardGeneric("dnaCases"))
setGeneric("dnaCases<-", function(x, value) standardGeneric("dnaCases<-"))

setMethod("dnaCases","outbreak", function(x, ...){
    return(dna(x, what="cases"))
})

setReplaceMethod("dnaCases","outbreak",function(x,value) {
    ## if no DNA
    if(is.null(dna(x))){
        return(NULL)
    }

    ## otherwise, test length of replacement if there was a (non-NULL) value before
    if(!is.null(dnaCases(x))){
        if(length(value)!=length(dnaCases(x))) stop(paste("Wrong length provided for replacement (old:",length(dnaCases(x))", new:",length(value)))
    }

    ## accept if this is OK
    slot(x,"dna.case",check=TRUE) <- as.character(value)

    ## return
    return(x)
})



## accessor: dnaDates
setGeneric("dnaDates", function(x, value) standardGeneric("dnaDates"))
setGeneric("dnaDates<-", function(x, value) standardGeneric("dnaDates<-"))

setMethod("dnaDates","outbreak", function(x, ...){
    return(dna(x, what="dates"))
})

setReplaceMethod("dnaDates","outbreak",function(x,value) {
    ## if no DNA
    if(is.null(dna(x))){
        return(NULL)
    }

    ## otherwise, test length of replacement if there was a (non-NULL) value before
    if(!is.null(dnaDates(x))){
        if(length(value)!=length(dnaDates(x))) stop(paste("Wrong length provided for replacement (old:",length(dnaDates(x))", new:",length(value)))
    }

    ## if this is OK, check type
    if(inherits(value, c("POSIXct","integer","numeric"))){
        slot(x,"dna.date",check=TRUE) <- as.character(value)
    } else {
        stop("Unknown type for replacement; accepted classes for dates are: POSIXct, integer or numeric")
    }

    ## return
    return(x)
})


## accessor: dnaDays
setGeneric("dnaDays", function(x, value) standardGeneric("dnaDays"))
setGeneric("dnaDays<-", function(x, value) standardGeneric("dnaDays<-"))

setMethod("dnaDays","outbreak", function(x, ...){
    return(dna(x, what="days"))
})

setReplaceMethod("dnaDays","outbreak",function(x,value) {
    ## if no DNA
    if(is.null(dna(x))){
        return(NULL)
    }

    ## otherwise, test length of replacement if there was a (non-NULL) value before
    if(!is.null(dnaDays(x))){
        if(length(value)!=length(dnaDays(x))) stop(paste("Wrong length provided for replacement (old:",length(dnaDays(x))", new:",length(value)))
    }

    ## if this is OK, check type
    if(inherits(value, c("integer","numeric"))){
        slot(x,"dna.day",check=TRUE) <- as.integer(value)
    } else {
        stop("Unknown type for replacement; accepted classes for days are: integer or numeric")
    }

    ## return
    return(x)
})





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




