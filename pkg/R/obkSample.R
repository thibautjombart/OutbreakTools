
############################
####  CLASSE DEFINITION ####
############################

## CLASS DESCRIPTION:
## - instance of obkSample stores data on samples collected on individuals
## - samples are stored as a (possibly named) list
## - names of the list are the names of the different elements

setClass("obkSample", representation(sampleID="characterOrNULL", colldate="DateOrNull", type="characterOrNULL", outcome="listOrNULL",seqlist="listOrNULL"), prototype(sampleID=NULL,colldate=NULL,type=NULL,outcome=NULL,seqlist=NULL))





######################
####  CONSTRUCTOR ####
######################

## INPUT DESCRIPTION:
## sampleID: a character defining the ID provided by the user
## colldate: a Date object indicating the collection date
##         :  if not specified, it will try "%Y-%m-%d" then "%Y/%m/%d" (from Date package)
## sampletype: a character indicating the type of sample ("nasal", "blood", "pcr", "serology", "sequence")
## outcomes: a list specifying the result of the sample eg. concentration, positive/negative etc.
## assaytypes: the type of assay is defined as the name of elements of the list
## sequences: a object obkSequences
setMethod("initialize", "obkSample", function(.Object, sampleID=NULL, colldate=NULL, sampletype=NULL, outcomes=NULL, assaytypes=NULL, seqlist=NULL) {

    ## RETRIEVE PROTOTYPED OBJECT ##
    x <- .Object

    ## escape if no info provided ##
    if(is.null(sampleID) && is.null(colldate) && is.null(sampletype) && is.null(outcomes)&& is.null(assaytypes) && is.null(sequences) )
      return(x)


    ## PROCESS ARGUMENTS ##

    if(!is.null(sampleID))
    {
        sampleID <- as.character(sampleID)[1]
    }

    if(!is.null(colldate))
    {
      ## check that colldate can be converted ##
      ## what about specifying Date format?
      ##if(!is.Date(Date) || !inherits(dna, "DNAbin")) stop("dna input could not be processed into a DNAbin list")
      collDate <- as.Date(colldate)[1] 
        
    }
    
    if(!is.null(sampletype))
    {
        sampletype <- as.character(sampletype)[1]
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
    x@sampleID <- sampleID
    x@colldate <- colldate
    x@sampletype <- sampletype
    if(is.null(assaytypes))
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

## sampleID: a character defining the ID provided by the user
## colldate: a Date object indicating the collection date
##         :  if not specified, it will try "%Y-%m-%d" then "%Y/%m/%d" (from Date package)
## type: a character indicating the type of sample ("nasal", "blood", "pcr", "serology", )
## outcome: a list (??? or vector of numerics) specifying the result of the sample eg. concentration, positive/negative etc. 
## seqlist: a list of sequences

################
## get.sampleID ##
################
setMethod("get.sampleID","obkSample", function(x, ...){
    if(is.null(x@sampleID)) return(NULL)
    return(x@sampleID)
})

################
## get.colldate ##
################
setMethod("get.colldate","obkSample", function(x, ...){
  if(is.null(x@colldate)) return(NULL)
  return(x@colldate)
})

################
## get.type ##
################
setMethod("get.sampletype","obkSample", function(x, ...){
  if(is.null(x@sampletype)) return(NULL)
  return(x@sampletype)
})

################
## get.noutcomes ##
################
setMethod("get.noutcomes","obkSample", function(x, ...){
  if(is.null(x@outcomes)) return(0)
  return(length(x@outcomes))
})

################
## get.outcomes ##
################
setMethod("get.outcomes","obkSample", function(x, index=NULL,...){
  if(is.null(x@outcomes)) return(NULL)
  if(is.null(index))  return(x@outcomes)
  else
    {
      if(is.numeric(index) && index>=1 && index<=get.noutcomes(x)) return(x@outcome[[index]]) 
      else
      {
        z<-match(index,names(x@outcomes))
        if(!is.na(z)) return(x@outcomes[[z]])
        else stop("index must be either an integer between 1 and number of outcomes or an existing assaytype")
      }
    }
})

################
## get.assaytypes ##
################
setMethod("get.assaytypes","obkSample", function(x, index=NULL,...){
  if(is.null(x@outcomes)) return(NULL)
  if(is.null(index))  return(names(x@outcomes))
  else
  {
    if(is.numeric(index) && index>=1 && index<=get.noutcomes(x)) return(names(x@outcome)[index]) 
    else stop("index must be an integer between 1 and number of outcomes")
  }
})


################
## is.assaytype.done ##
################
setMethod("is.assaytype.done","obkSample", function(x, assaylabel=NULL,...){
  if(is.null(x@outcomes)) return(FALSE)
  if(is.null(assaylabel))
    stop("assay label is missing")
  else
  {
    z<-match(assaylabel,names(x@outcomes))
    if(!is.na(z))
      {
      if(!is.na(x@outcomes[[z]]))
        return (TRUE)
      else return(FALSE)
    }
    else stop("assaylabel must be an existing assaytype")
  }
})


####################
## get.nsequences ##
####################
setMethod("get.nsequences","obkSample", function(x, ...){
  out<-get.nsequences(x@sequences)
    return(out)
})

####################
## get.sequences ##
####################
setMethod("get.sequences","obkSample", function(x, ...){
  out<-get.sequences(x@sequences)
  return(out)
})


################
## get.nlocus ##
################
setMethod("get.nlocus","obkSample", function(x, ...){
  return(get.nlocus(x@sequences))
})



################
## get.locus ##
################
setMethod("get.locus","obkSample", function(x, ...){
  return(get.locus(x@sequences)
})



#############
## get.dna ##
#############
## returns a matrix of dna sequences for a given locus
setMethod("get.dna","obkSample", function(x, locus=NULL, ...){
  return(get.dna(x@sequences)
})



######################
####  SHOW METHOD ####
######################

setMethod ("show", "obkSample", function(object){
    sampleID <- get.sampleID(object)
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
