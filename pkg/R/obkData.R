
############################
####  CLASSE DEFINITION ####
############################

## CLASS DESCRIPTION:
## Instance of obkData store outbreak data; its content includes:
## - @individuals: a list of obkIndividual
## - @meta: meta-information on the individuals (group, etc.), as a data.frame
## - @contacts: contact information as obkContacts
setClass("obkData", representation(individuals="listOrNULL", meta="dataFrameOrNULL", contacts="obkContacts"),
         prototype(individuals=NULL, meta=NULL, contacts=NULL))







######################
####  CONSTRUCTOR ####
######################

## INPUT DESCRIPTION:
## data: a data.frame where each row is a sample of a given type, at a given data, and the following (optional) columns:
## - individualID
## - sampleID
## - colldate
## - outcome
## - assaytype
##
## meta: a data.frame with any information about the individuals
## locus: a vector of characters indicating which locus each sequence corresponds to
setMethod("initialize", "obkData", function(.Object, data=NULL, meta=NULL, contacts=NULL){

    ## RETRIEVE PROTOTYPED OBJECT ##
    x <- .Object

    ## escape if no info provided ##
    if(is.null(data)) return(x)


    ## PROCESS INFORMATION TO CREATE INDIVIDUALS ('data') ##
    ## check that relevant fields are here ##
    if("individualID" %in% names(data)) stop("no field 'individualID' in the sample data.frame ('data')")
    if("sampleID" %in% names(data)) stop("no field 'sampleID' in the sample data.frame ('data')")
    if("colldate" %in% names(data)) stop("no field 'colldate' in the sample data.frame ('data')")
    if("outcome" %in% names(data)) stop("no field 'outcome' in the sample data.frame ('data')")
    if("assaytype" %in% names(data)) stop("no field 'assaytype' in the sample data.frame ('data')")

    ## here, we just lapply the constructor for obkIndividuals
    ## the constructor has to take as argument a data.frame with columns:
    ## individualID, sampleID, colldate, outcome, assaytype
    indivID <- data$individualID
    data <- data[, !"individualID" %in% names(data), drop=FALSE]
    x@individuals <- lapply(split(data, indivID), new)


    ## PROCESS META-INFORMATION ABOUT INDIVIDUALS ('meta') ##
    ## check number of rows
    meta <- as.data.frame(data)
    if(nrow(meta)!=length(x@individuals)) stop("meta information should have one row")

    ## PROCESS INFORMATION ABOUT CONTACTS ('contacts') ##
    ## need to make sure that contact input is consisten with constructor
    x@contacts <- new("obkContacts", contacts)


    ## RETURN OBJECT ##
    return(x)
}) # end obkData constructor








####################
####  ACCESSORS ####
####################

################
## get.nlocus ##
################
setMethod("get.nlocus","obkData", function(x, ...){
    return(sum(sapply(x@individuals, get.nlocus)))
})


####################
## get.nsequences ##
####################
setMethod("get.nsequences","obkData", function(x, ...){
    return(sum(sapply(x@individuals, get.nsequences)))
})


######################
## get.nindividuals ##
######################
setMethod("get.nindividuals","obkData", function(x, ...){
    return(length(x@individuals, get.nsequences))
})


##################
## get.nsamples ##
##################
setMethod("get.nsamples","obkData", function(x, ...){
    return(lapply(x@individuals, get.nsamples))
})


## ################
## ## get.locus ##
## ################
## setMethod("get.locus","obkData", function(x, ...){
##     return(get.locus(get.dna(x)))
## })



## #############
## ## get.dna ##
## #############
## ## returns a matrix of dna sequences for a given locus
## setMethod("get.dna","obkData", function(x, locus=NULL, ...){
##     return(get.dna(get.dna(x)))
## })








## ######################
## ####  SHOW METHOD ####
## ######################

## setMethod ("show", "obkData", function(object){
##     nLoc <- get.nlocus(object)
##     nSeq <- get.nsequences(object)
##     seqword <- ifelse(nLoc>1, "sequences", "sequence")
##     locword <- ifelse(nLoc>1, "loci", "locus")
##     cat(paste("\n =", nSeq,"DNA", seqword, "in", nLoc, "loci =\n\n"))
##     if(nLoc>0) print(object@dna)
## })







##################
####  TESTING ####
##################
## NOTE: THIS MUST BE COMMENTED WHEN COMPILING/INSTALLING THE PACKAGE

