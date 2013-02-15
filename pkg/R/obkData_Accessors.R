
###############################
#### ACCESSORS FOR OBKDATA ####
###############################

###############
## get.locus ##
###############
setMethod("get.locus", "obkData", function(x, ...){
    if(is.null(x@dna)) return(NULL)
    return(get.locus(x@dna))
})



################
## get.nlocus ##
################
setMethod("get.nlocus", "obkData", function(x, ...){
    if(is.null(x@dna)) return(0)
    return(get.nlocus(x@dna))
})



####################
## get.nsequences ##
####################
setMethod("get.nsequences", "obkData", function(x, ...){
    if(is.null(x@dna)) return(0)
    return(get.nsequences(x@dna))
})



#############
## get.dna ##
#############
setMethod("get.dna", "obkData", function(x, locus=NULL, id=NULL, ...){
    ## checks and escapes ##
    if(is.null(x@dna)) return(NULL)
    return(get.dna(x@dna, locus=locus, id=id, ...))
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









###############################
#### SUBSETTING PROCEDURES ####
###############################

###################
## subset method ##
###################
setMethod("subset", "obkData", function(x, individuals=NULL, samples=NULL, ...){
    ## CHECK THAT REQUESTED INFOR IS THERE ##
    if(is.null(x@individuals)) individuals <- NULL
    if(is.null(x@samples)) samples <- NULL

    ## TRIVIAL SUBSET: ALL KEPT ###
    if(is.null(individuals) && is.null(samples)) return(x)

    ## SUBSET BY INDIVIDUALS ##
    if(!is.null(individuals)){
        ## check that indicated indiv are known
        if(!all(individuals %in% get.individuals(x))){
            temp <- paste(individuals[!individuals %in% get.individuals(x)], collapse=", ")
            warning(paste("The following individuals were not found in the data:", temp))
            individuals <- individuals[individuals %in% get.individuals(x)]
        }

        ## subset @individuals
        x@individuals <- x@individuals[individuals, ,drop=FALSE]

        ## subset @samples
        x@samples <- x@samples[x@samples$individualID %in% individuals, ,drop=FALSE]

        ## subset @dna
        x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @contacts
        ## (TODO)

        ## subset @clinicals
        ## (TODO)

        ## subset @trees
        ## (TODO)

    } # end processing 'individuals' argument


    ## SUBSET BY SAMPLE ##
    if(!is.null(samples)){
        ## check that indicated indiv are known
        if(!all(samples %in% x@samples$sampleID)){
            temp <- paste(samples[!samples %in% x@samples$sampleID], collapse=", ")
            warning(paste("The following samples were not found in the data:", temp))
            samples <- samples[samples %in% x@samples$sampleID]
        }

        ## subset @samples
        x@samples <-x@samples[x@samples$sampleID %in% samples, ,drop=FALSE]

        ## subset @individuals
        x@individuals <-x@individuals[rownames(x@individuals) %in% x@samples$individualID,,drop=FALSE]

        ## subset @dna
        x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @contacts
        ## (TODO)

        ## subset @clinicals
        ## (TODO)

        ## subset @trees
        ## (TODO)

    } # end processing 'samples' argument


    return(x)
}) # end subset method




