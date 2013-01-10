
############################
####  CLASSE DEFINITION ####
############################

## CLASS DESCRIPTION:
## - instance of obkSample stores data on samples collected on individuals
## - samples are stored as a (possibly named) list
## - names of the list are the names of the different elements

setClass("obkSample", representation(labID="characterOrNULL", colldate="DateOrNull", type="characterOrNULL", outcome="listOrNULL",seqlist="listOrNULL"), prototype(labID=NULL,colldate=NULL,type=NULL,outcome=NULL,seqlist=NULL))





######################
####  CONSTRUCTOR ####
######################

## INPUT DESCRIPTION:
## labID: a character defining the ID provided by the user
## colldate: a Date object indicating the collection date
##         :  if not specified, it will try "%Y-%m-%d" then "%Y/%m/%d" (from Date package)
## type: a character indicating the type of sample ("nasal", "blood", "pcr", "serology", "sequence")
## outcomes: a list specifying the result of the sample eg. concentration, positive/negative etc.
## assaytypes: the type of assay is defined as the name of elements of the list
## sequences: a object obkSequences
setMethod("initialize", "obkSample", function(.Object, labID=NULL, colldate=NULL, type=NULL, outcomes=NULL, assaytypes=NULL, seqlist=NULL) {

    ## RETRIEVE PROTOTYPED OBJECT ##
    x <- .Object

    ## escape if no info provided ##
    if(is.null(labID) && is.null(colldate) && is.null(type) && is.null(outcomes)&& is.null(assaytypes) && is.null(sequences) )
      return(x)


    ## PROCESS ARGUMENTS ##

    if(!is.null(labID))
    {
        labID <- as.character(labID)[1]
    }

    if(!is.null(colldate))
    {
      ## check that colldate can be converted ##
      ## what about specifying Date format?
      ##if(!is.Date(Date) || !inherits(dna, "DNAbin")) stop("dna input could not be processed into a DNAbin list")
      collDate <- as.Date(colldate)[1] 
        
    }
    
    if(!is.null(type))
    {
        type <- as.character(type)[1]
    }

    if(!is.null(outcomes))
    {
        outcomes <- as.list(outcomes) 
        # names of outcomes are the descriptions of corresponding assaytypes
    }
    
    if(!is.null(seqlist))
    {
      x@sequences <- new("obkSequences",sequences) # to avoid unecessary copy of sequences that might be very big
    }
    
    ## should we deal with the different formats for all the variables here?
    ## more checkings?    

    ## SHAPE OUTPUT ##
    x@labID <- labID
    x@colldate <- colldate
    x@type <- type
    if(is.null(assays))
      x@outcomes <- outcomes
    else if(length(outcomes) != length(assaytypes)) 
      stop(paste("Length mismatch (outcomes:", length(outcomes), "items; assaytypes:", length(assaytypes),"items)"))
    else 
      names(x@outcomes) <- assaytypes
        
    return(x)
}) # end obkSample constructor





####################
####  ACCESSORS ####
####################

## labID: a character defining the ID provided by the user
## colldate: a Date object indicating the collection date
##         :  if not specified, it will try "%Y-%m-%d" then "%Y/%m/%d" (from Date package)
## type: a character indicating the type of sample ("nasal", "blood", "pcr", "serology", )
## outcome: a list (??? or vector of numerics) specifying the result of the sample eg. concentration, positive/negative etc. 
## seqlist: a list of sequences

################
## get.labID ##
################
setMethod("get.labID","obkSample", function(x, ...){
    if(is.null(x@labID)) return(0)
    return(x@labID)
})

################
## get.colldate ##
################
setMethod("get.colldate","obkSample", function(x, ...){
  if(is.null(x@colldate)) return(0)
  return(x@colldate)
})

################
## get.type ##
################
setMethod("get.type","obkSample", function(x, ...){
  if(is.null(x@type)) return(0)
  return(x@type)
})

################
## get.noutcomes ##
################
setMethod("get.noutcomes","obkSample", function(x, ...){
  if(is.null(x@outcome)) return(0)
  return(length(x@outcome))
})

################
## get.outcome ##
################
setMethod("get.outcome","obkSample", function(x, index=NULL,...){
  if(is.null(x@outcome)) return(0)
  if(is.null(index))  return(x@outcome)
  else
    {
    if(index>=1 && index<=get.noutcomes(x)) return(x@outcome[[index]]) 
    else stop("index must be an integer between 1 and number of outcomes")
    }
})

####################
## get.nsequences ##
####################
setMethod("get.nsequences","obkSample", function(x, ...){
    
    return(length(x@seqlist))
})

####################
## get.sequences ##
####################
setMethod("get.sequences","obkSample", function(x, ...){
  
  return(x@seqlist)
## shall we heritate from the sequence class method here?
  })



######################
####  SHOW METHOD ####
######################

setMethod ("show", "obkSample", function(object){
    labID <- get.labID(object)
    colldate <- get.colldate(object)
    type<-get.type(object)
    noutcomes<-get.noutcomes(object)
    outcome<-get.outcome(object)
    nseq<-get.nsequences(object)
    seq<-get.sequence(object)
    seqword <- ifelse(nLoc>1, "sequences", "sequence")
    cat(paste("\n =", nSeq,"DNA", seqword, "in", nLoc, " loci =\n\n"))
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
