
#################################
####  DEFINITIONS OF CLASSES ####
#################################

##################
## outbreak class
##################
setOldClass("DNAbin")
setOldClass("POSIXct")
setClassUnion("characterOrNULL", c("character","NULL"))
setClassUnion("integerOrNULL", c("integer","NULL"))
setClassUnion("numericOrNULL", c("numeric","NULL"))
setClassUnion("matrixOrNULL", c("matrix","NULL"))
setClassUnion("DNAbinOrNULL", c("DNAbin","NULL"))
setClassUnion("date", c("POSIXct","NULL"))
##setClassUnion("date", c("POSIXct","numeric","integer","NULL"))

## class description:
## @n.patients: number of patients reported
## @n.swabs: number of swabs collected
## @n.dna: number of DNA sequences collected
## @patient.id: identifier (labels) of the patients; defines patient ordering (e.g., 1st label is first patient, etc.)
## @swabs: a vector indicating swab results as numeric data
## @swab.patients: label of the patient of each swab in '@swabs'; values must have a match in '@patient.id'
## @swab.types: type of swab for each value of '@swabs'; could be body location, technique used, etc.
## @swab.dates: dates of the swabs in '@swabs', in POSIXct format
## @swab.days:  dates of the swabs in '@swabs', in number of days from the first data (swab or DNA sequence) collected
## @swab.mat: matrix indicating positive/negative swabs, with patient/swab type in rows and dates (in days from first data) in columns
## @dna: DNA sequences sampled for some or all patients, as a DNAbin object matrix
## @dna.patients: indicator of the patient (i.e., patient) for each row of '@dna'; values must have a match in '@patient.id'
## @dna.types: indicator of the type of swab for each row of '@dna'
## @dna.dates: collection date of each DNA sequence in '@dna', in POSIXct format
## @dna.days: integer collection date of each DNA sequence in '@dna'; '0' is the first swab or sequence collected

setClass("outbreak", representation(n.patients="integer", n.swabs="integer", n.dna="integer", patient.id="characterOrNULL",
                                    swabs="numericOrNULL", swab.patients="characterOrNULL", swab.types="characterOrNULL",
                                    swab.dates="date", swab.days="integerOrNULL", swab.mat="matrixOrNULL",
                                    dna="DNAbinOrNULL", dna.patients="characterOrNULL", dna.types="characterOrNULL",
                                    dna.dates="date", dna.days="integerOrNULL"),
         prototype(n.patients=0L, n.swabs=0L, n.dna=0L, patient.id=NULL,
                   swabs=NULL, swab.mat=NULL, swab.patients=NULL, swab.types=NULL, swab.dates=NULL, swab.days=NULL,
                   dna=NULL, dna.patients=NULL, dna.types=NULL, dna.dates=NULL, dna.days=NULL))







#######################
####  CONSTRUCTORS ####
#######################

########################
## outbreak constructor
########################
##
## input is a named list with the following possible content:
##
## $swabs: a matrix with 3 columns, each row being a swab, with:
## - col.1: the patient/patient id (will be converted to characters)
## - col.2: the date; can be POSIXt, or characters
## characters will be converted to POSIXct and are expected to follow the default format: yyyy-mm-dd
## see ?format.POSIXct for more information
## - col.3: the swab result, as integer or numeric
## - col.4: (optional) a character indicating the type of swab
##
## $dna: a DNAbin matrix containing aligned DNA sequences
## $dna.dates: a vector of collection dates for the DNA sequences; format expected as for swab data
## $dna.patients: a vector of patient/patient id for the DNA sequences
##

setMethod("initialize", "outbreak", function(.Object, swabs=NULL, dna=NULL, dna.patients=NULL, dna.types=NULL,
                                             dna.dates=NULL, date.format="%Y-%m-%d", ...) {
    ## RETRIEVE EMPTY OBJECT ##
    x <- .Object

    ## HANDLE ARGUMENTS ##
    ## swabs
    if(!is.null(swabs)){
        swabs <- as.data.frame(swabs, stringsAsFactors=FALSE)

        ## swabs
        if(is.factor(swabs[,3])) swabs[,3] <- as.numeric(as.character(swabs[,3]))

        ## swab.dates
        swab.dates <- swabs[,2]
        if(is.factor(swab.dates)) swab.dates <- as.character(swab.dates)
        if(is.character(swab.dates)) swab.dates <- as.POSIXct(swab.dates, format=date.format)
        if(!inherits(swab.dates, "POSIXct")) stop("dates in 'swabs' must be provided as character (yyyy-mm-dd) or POSIXct")

        ## swab.patients
        swab.patients <- as.character(swabs[,1])

        ## swab.types
        if(ncol(swabs)>3) {
            swab.types <- as.character(swabs[,4])
        } else {
            swab.types <- rep("swab", nrow(swabs))
        }
    } else {
        swab.dates <- NULL
        swab.patients <- NULL
        swab.types <- NULL
    }

    ## dna
    if(!is.null(dna)){
        if(is.character(dna)) dna <- as.DNAbin(tolower(dna))
        if(is.list(dna)) dna <- as.matrix(dna)
        if(!inherits(dna, "DNAbin") | !is.matrix(dna)) stop("dna data could not be converted to a DNAbin matrix")

        ## dna.dates
        if(!is.null(dna.dates)){
            if(is.character(dna.dates)) dna.dates <- as.POSIXct(dna.dates, format=date.format)
            if(!inherits(dna.dates, "POSIXct")) stop("dates in 'dna.dates' must be provided as character (yyyy-mm-dd) or POSIXct")
            if(length(dna.dates) != nrow(dna)) stop(paste("Wrong length for dna.dates (dna:", nrow(dna), "sequences, dna.dates:", length(dna.dates),"dates)"))
        }

        ## dna.patients
        if(!is.null(dna.patients)){
            dna.patients <- as.character(dna.patients)
            if(length(dna.patients) != nrow(dna)) stop(paste("Wrong length for dna.patients (dna:", nrow(dna), "sequences, dna.patients:", length(dna.patients),"labels)"))
        }

    } else {
        dna.dates <- NULL
        dna.patients <- NULL
    }

    ## get range of dates ##
    if(!is.null(swab.dates) && !is.null(dna.dates)) range.dates <- range(c(swab.dates, dna.dates))
    if(!is.null(swab.dates) && is.null(dna.dates)) range.dates <- range(swab.dates)
    if(is.null(swab.dates) && !is.null(dna.dates)) range.dates <- range(dna.dates)


    ## FILL IN OBJECT ##
    ## swabs
    if(!is.null(swabs)){
        n.dates <- as.numeric(difftime(range.dates[2], range.dates[1], unit="days"))+1
        n.row <- length(unique(paste(swab.patients, swab.types)))

        ## create empty matrix
        x@swab.mat <- matrix(as.numeric(NA), ncol=n.dates, nrow=n.row)
        colnames(x@swab.mat) <- 0:(n.dates-1)
        patientsNtypes <- paste(swab.patients, swab.types, sep="-")
        rownames(x@swab.mat) <- unique(patientsNtypes)

        ## get swab.days
        x@swab.days <- as.integer(difftime(swab.dates, range.dates[1], unit="days"))

        ## fill in the matrix
        for(i in 1:nrow(swabs)){
            x@swab.mat[patientsNtypes[i] , x@swab.days[i]+1] <- swabs[i, 3]
        }

        ## fill in the rest of swab information
        x@swabs <- as.numeric(swabs[,3])
        x@n.swabs <- nrow(swabs)
        x@swab.patients <- swab.patients
        x@swab.dates <- swab.dates
        x@swab.types <- swab.types
    }

    ## dna
    if(!is.null(dna)){
        x@dna <- dna
        x@dna.dates <- dna.dates
        if(!is.null(dna.dates)){
            x@dna.days <- as.integer(difftime(dna.dates, range.dates[1], unit="days"))
        }
        x@dna.patients <- dna.patients
        x@n.dna <- nrow(dna)
    }

    ## general stuff
    if(!is.null(swabs) | !is.null(dna.patients)){
        x@patient.id <- sort(unique(c(x@swab.patients, x@dna.patients)))
        x@n.patients <- length(x@patient.id)
    }

    return(x)
}) # end outbreak constructor


## CODE FOR BASIC TEST
## library(ape)
## swabs <- data.frame(c("pat1", "pat2", "pat-3", "pat-3"), c("2001-01-10","2001-02-23","2001-01-02","2001-03-05"), c(1,1,0,1))
## colnames(swabs) <- c("patient","date","result")
## data(woodmouse)
## dna <- woodmouse[1:2,]
## dna.dates <- c("2001-01-10", "2001-03-07")
## dna.patients <- c("pat1", "pat-3")

## new("outbreak")
## new("outbreak", swabs=swabs)
## new("outbreak", dna=dna)
## new("outbreak", dna=dna, dna.pat=dna.patients, dna.dat=dna.dates)
## x <- new("outbreak", swabs=swabs, dna=dna, dna.pat=dna.patients, dna.dat=dna.dates)
## x








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




