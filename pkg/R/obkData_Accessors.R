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



######################
## get.nindividuals ##
######################
setMethod("get.nindividuals", "obkData", function(x, data=c("samples", "individuals"), ...){
    data <- match.arg(data)

    return(length(get.individuals(x, data=data)))
})



##############
## get.data ##
##############
##
## Universal accessor:
## tries to find any type of data within the obkData object
##
setMethod("get.data", "obkData", function(x, data, drop=TRUE, ...){
    data <- as.character(data)

    ## LOOK FOR SLOT NAMES ##
    if(data[1] %in% slotNames(x)) return(slot(x, data))

    ## LOOK FOR 'DATA' IN INDIVIDUALS ##
    if(!is.null(x@individuals)){
        if(any(data %in% names(x@individuals))){
            return(x@individuals[,data,drop=drop])
        }
    }

    ## LOOK FOR 'DATA' IN SAMPLES ##
    if(!is.null(x@samples)){
        if(any(data %in% names(x@samples))){
            return(x@samples[,data,drop=drop])
        }
    }

    ## DEFAULT IF WE DON'T KNOW WHAT TO RETURN ##
    warning(paste("data '", data, "'was not found in the object"))
    return(NULL)
})



