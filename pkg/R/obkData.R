
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
setClass("obkData", representation(individuals="dataframeOrNULL", samples="dataframeOrNULL", clinical="obkClinicalEventOrNULL", dna="listOrNULL", contacts="obkContactsOrNULL"),
         prototype(individuals=NULL, samples=NULL, dna=NULL, clinical=NULL, contacts=NULL))







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
## - "sequenceID": optional but particular processing by the constructor, a sequence ID existing in 'dna'
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
    if(is.null(individuals) && is.null(samples) && is.null(clinical) && is.null(dna) && is.null(contacts)) return(x)


    ## PROCESS INFORMATION TO CREATE INDIVIDUALS ('data') ##
    ## coerce to data.frames
    if(!is.null(individuals)) individuals <- as.data.frame(individuals)
    if(!is.null(samples)) samples <- as.data.frame(samples)
    if(!is.null(clinical)) clinical <- as.data.frame(clinical)
    if(!is.null(dna) && (inherits(dna, "DNAbin") && is.matrix(dna))) dna <- as.list(dna)
    if(!is.null(dna) && (!is.list(dna) || !inherits(dna, "DNAbin"))) stop("dna is not a list of DNAbin objects.")

    ## check that relevant fields are here ##
    if("individualID" %in% names(individuals)) stop("no field 'individualID' in the individuals data.frame ('individuals')")
    if("individualID" %in% names(samples)) stop("no field 'individualID' in the sample data.frame ('samples')")
    if("sampleID" %in% names(samples)) stop("no field 'sampleID' in the sample data.frame ('samples')")
    if("date" %in% names(samples)) stop("no field 'date' in the sample data.frame ('samples')")


    ## PROCESS INFORMATION ABOUT INDIVIDUALS ('individuals') ##
    if(!is.null(individuals)){
        lab.posi <- match("individualID", names(individuals))
        x@individuals <- cbind.data.frame(id=as.character(individuals[,lab.posi,drop=FALSE]), individuals[,-lab.posi,drop=FALSE])
    }


    ## PROCESS INFORMATION ABOUT SAMPLES ('SAMPLES') ##
    if(!is.null(samples)){
        x@samples <- samples[,c("individualID","sampleID","date"),drop=FALSE]
        x@samples[,"individualID"] <- as.character(x@samples[,"individualID"])
        x@samples[,"sampleID"] <- as.character(x@samples[,"sampleID"])
        x@samples[,"date"] <- as.Date(x@samples[,"date"])
        extraInfo <- samples[, !names(samples) %in% c("individualID","sampleID","date"), drop=FALSE]
        x@samples <- cbind.data.frame(x@samples, extraInfo)
    }

    ## PROCESS INFORMATION ABOUT CLINICAL ('clinicals') ##
    ## to be filled in by Paul & Marc

    ## PROCESS INFORMATION ABOUT CONTACTS ('contacts') ##
    ## need to make sure that contact input is consisten with constructor
    if(!is.null(contacts)){
        x@contacts <- new("obkContacts", contacts)
    }


    ## PROCESS INFORMATION ABOUT DNA SEQUENCES ('sequenceID') ##
    seqPos <- which(names(samples) %in% c("sequenceID"))
    if(length(seqPos)==0 || is.null(dna)){
        x@dna <- NULL
    } else {
        if(!all(samples$sequenceID %in% names(dna))) warning("some sequence ID where not present in the dna list")
        temp <- split(samples, samples$sampleID)

        ## small auxiliary function to pass relevant data to the constructor, and nothing otherwise
        ## (can't be using dna[NA])
        ## vecID: vector of sequence IDs (including possible NAs, can be NAs only)
        f1 <- function(vecID, locus){
            if(all()) return(NULL)
            toRemove <- is.na(vecID)
            vecID <- vecID[!toRemove]
            locus <- locus[!toRemove]
            return(new("obkSequences", dna=dna[vecID], locus=locus))
        }
        x@dna <- lapply(temp, function(e) f1(e$sequenceID, e$locus))
    }


    ## PROCESS INFORMATION ABOUT CLINICAL EVENTS ('clinical') ##
    ## to be filled in by Paul B


    ## PROCESS INFORMATION ABOUT CONTACTS ('contacts') ##
    ## to be filled in by Simon F


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

new("obkData")
