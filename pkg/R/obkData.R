
############################
####  CLASSE DEFINITION ####
############################

## CLASS DESCRIPTION:
## Instance of obkData store outbreak data; its content includes:
## - @data: data about samples, stored as a data.frame
## - @meta: meta-information on the individuals (group, etc.), stored as a data.frame
## - @clinical: information about interventions and events, stored as obkClinicalEvent
## - @dna: dna data, stored as a list of DNA sequences (list of DNAbin)
## - @contacts: contact information as obkContacts
setClass("obkData", representation(data="dataframeOrNULL", meta="dataframeOrNULL", dna="listOrNULL", clinical="obkClinicalEventOrNULL", contacts="obkContactsOrNULL"),
         prototype(data=NULL, meta=NULL, dna=NULL, clinical=NULL, contacts=NULL))







######################
####  CONSTRUCTOR ####
######################

## INPUT DESCRIPTION:
## 'individuals': a data.frame with any information on the individuals, each row being an individual, with the following columns:
## - "individualID"
## - any other named column
##
## 'samples': a data.frame where each row is an observation made on a sample, and the following mandatory columns:
## - "individualID"
## - "sampleID"
## - "date"
## - any optional, named column
## - "sequence": optional but particular processing by the constructor, a sequence ID existing in 'dna'
## - "locus": optional but particular processing by the constructor, the locus of a sequence
##
## 'dna': a DNAbin list with named sequences
##
## 'clinical': information about clinical events stored as a data.frame
##
## 'contacts': whatever Simon Frost has in mind
##
setMethod("initialize", "obkData", function(.Object, individuals=NULL, samples=NULL, clinical=NULL, dna=NULL, contacts=NULL){

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

