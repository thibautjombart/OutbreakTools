############################
####  CLASSE DEFINITION ####
############################

## CLASS DESCRIPTION:
## - instance of obkClinicalEvent store a clinical event associated with an individual
## - individual.ID is the ID of the individual to which the clinical event refers to
## - type is the type of clinical event from a constraint list
## - start.date is the starting date
## - end.date is the ending date
## - location is the location of the event
## - non defined elements are set as NULL

setClass("obkClinicalEvent", representation(individual.ID="characterOrNULL", type="characterOrNULL", start.date="DateOrNULL", end.date="DateOrNULL", duration="numericOrNULL", location="characterOrNULL"), prototype(individual.ID=NULL, type=NULL, start.date=NULL, end.date=NULL, start.date=NULL, duration=NULL, location=NULL))


######################
####  CONSTRUCTOR ####
######################

## INPUT DESCRIPTION:
## individual.ID: a character
## type : a character
## start.date : a Date
## end.date : a Date
## duration : a numeric
## location : a character
setMethod("initialize", "obkClinicalEvent", function(.Object, individual.ID=NULL, type=c("hospitalisation", "vaccination", "treatment"), start.date=NULL, end.date=NULL, duration=NULL, location=NULL, format.Date="%Y-%m-%d") {

    ## RETRIEVE PROTOTYPED OBJECT ##
    x <- .Object

    ## escape if no type info provided ##
    if(is.null(type)) return(x)


    ## PROCESS ARGUMENTS ##
    ## check that the type is valid ##
    x@type <- match.arg(type)

    ## force character type if the ID is not NULL
    if(!is.null(individual.ID)) x@individual.ID <- as.character(individual.ID)

    ## force start.date in the standard date format
    if(!is.null(start.date)) x@start.date <- as.Date(start.date, format=format.Date)
    
    ## force start.date in the standard date format
    if(!is.null(end.date)) x@end.date <- as.Date(end.date, format=format.Date)
    
    ## force duration in the numeric format
    if(!is.null(duration)) x@duration <- as.numeric(duration)

    ## force character type if the location is not NULL
    if(!is.null(location)) x@location <- as.character(location)
    
    return(x)
    
}) # end obkClinicalEvent constructor