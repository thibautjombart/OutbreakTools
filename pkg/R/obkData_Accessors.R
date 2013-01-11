################################
#### ACCESSORS  FOR OBKDATA ####
################################

###############
## get.locus ##
###############
setMethod("get.locus", "obkData", function(x, ...){
  if(is.null(x@dna)) return(NULL)
  return(unique(unlist(lapply(x@dna, get.locus))))
})



################
## get.nlocus ##
################
setMethod("get.nlocus", "obkData", function(x, ...){
  if(is.null(x@dna)) return(0)
  return(length(unique(get.locus(x))))
})



####################
## get.nsequences ##
####################
setMethod("get.nsequences", "obkData", function(x, ...){
    if(is.null(x@dna)) return(0)
    return(sum(unlist(lapply(x@dna, get.nsequences))))
})



#############
## get.dna ##
#############
setMethod("get.dna", "obkData", function(x, locus=NULL, ...){
    ## checks and escapes ##
    if(is.null(x@dna)) return(NULL)
    if(get.nlocus(x)==0) return(NULL)
    if(get.nlocus(x)>1 && is.null(locus)) stop("there are several loci in the data - locus must be provided")

    ## get all sequences of the locus
    out <- lapply(x@dna, get.dna, locus=locus)
    ## remove NULL data
    out <- out[!sapply(out, is.null)]
    out <- Reduce(rbind, out)
    return(out)
})



#####################
## get.individuals ##
#####################
setMethod("get.individuals", "obkData", function(x, data=c("samples", "individuals"), ...){
    data <- match.arg(data)

    ## list individuals in @samples
    if(data=="samples"){
        if(is.null(x@samples)) return(NULL)
        return(unique(x@samples$individualID))
    }

    ## list individuals in @individuals
    if(data=="individuals"){
        if(is.null(x@individuals)) return(NULL)
        return(row.names(x@individuals))
    }
})




#####################
## get.nindividuals ##
#####################
setMethod("get.nindividuals", "obkData", function(x, data=c("samples", "individuals"), ...){
    data <- match.arg(data)

    return(length(get.individuals(x, data=data)))
})



## #####################
## ## get.individuals ##
## #####################
## setMethod("get.individuals", "obkData", function(x, individual = NULL, ...){

##   ## return NA if no info
##   if(is.null(x@individuals)) return(NA)

##   nInd <- get.nindividuals(x)
##   ## return only individual if
##   if(!is.null(individual)){
##    return(x)

##    ## otherwise use information attached to each individual
##    if(is.null(individual)) stop("individual must be specified (data contain more than one individual)")

##    ## return new obkData object subsetted according to the individual ID
##    ## return(new("obkData"))?

## })
