
############################
####  CLASSE DEFINITION ####
############################

## CLASS DESCRIPTION:
## - instance of obkIndividual store data about an individual
## - samples: a list of samples
## - clinicalEvents: a list of clinical events
## - NB: covariates about the individual are stored in a separate data frame


setClass("obkIndividual", representation(samples="obkSample",clinicalEvents="obkClinicalEvent"), prototype(samples=NULL,clinicalEvents=NULL))


######################
####  CONSTRUCTOR ####
######################

## INPUT DESCRIPTION:
## - sample: a list of samples
## - clinicalEvent: a list of clinical events
setMethod("initialize", "obkIndividual", function(.Object,samples=NULL,clinicalEvents=NULL) {

    ## RETRIEVE PROTOTYPED OBJECT ##
    x <- .Object

    return(x)
}) 




