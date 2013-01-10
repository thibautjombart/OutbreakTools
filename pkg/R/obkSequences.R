
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








####################
####  ACCESSORS ####
####################

################
## get.nlocus ##
################
setMethod("get.nlocus","obkSequences", function(x, ...){
    if(is.null(x@dna)) return(0)
    return(length(x@dna))
})



####################
## get.nsequences ##
####################
setMethod("get.nsequences","obkSequences", function(x, ...){
    nLoc <- get.nlocus(x)
    if(nLoc==0) return(0)

    out <- sum(sapply(x@dna, nrow))
    return(out)
})



################
## get.locus ##
################
setMethod("get.locus","obkSequences", function(x, ...){
    if(is.null(x)) return(NULL)
    return(names(x@dna))
})



#############
## get.dna ##
#############
## returns a matrix of dna sequences for a given locus
setMethod("get.dna","obkSequences", function(x, locus=NULL, ...){
    nLoc <- get.nlocus(x)

    ## return NULL if no info ##
    if(nLoc==0) return(NULL)

    ## return only locus if nLoc==1 ##
    if(nLoc==1) return(x@dna[[1]])

    ## otherwise use locus info ##
    if(is.null(locus)) stop("locus must be specified (data contain more than one locus)")

    return(x@dna[[locus]])
})








######################
####  SHOW METHOD ####
######################

setMethod ("show", "obkSequences", function(object){
    nLoc <- get.nlocus(object)
    nSeq <- get.nsequences(object)
    seqword <- ifelse(nLoc>1, "sequences", "sequence")
    locword <- ifelse(nLoc>1, "loci", "locus")
    cat(paste("\n =", nSeq,"DNA", seqword, "in", nLoc, "loci =\n\n"))
    if(nLoc>0) print(object@dna)
})







##################
####  TESTING ####
##################
## NOTE: THIS MUST BE COMMENTED WHEN COMPILING/INSTALLING THE PACKAGE

## library(ape)
## data(woodmouse)

## ## test constructor / show
## new("obkSequences") # empty object
## new("obkSequences", woodmouse) # no locus info
## new("obkSequences", as.matrix(woodmouse), locus=rep(c('loc1', 'loc2', 'locXX'), c(10,4,1)))


## ## test accessors
## x <- new("obkSequences", as.matrix(woodmouse), locus=rep(c('loc1', 'loc2', 'locXX'), c(10,4,1)))
## get.dna(x, locus=1)
## get.dna(x, locus="locXX")
## get.nlocus(x)
## get.nsequences(x)
