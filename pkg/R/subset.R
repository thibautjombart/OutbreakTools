

###############################
#### SUBSETTING PROCEDURES ####
###############################

###################
## obkData method ##
###################
setMethod("subset", "obkData", function(x, individuals=NULL, samples=NULL, date.from=NULL, date.to=NULL, date.format="",
                                        locus=NULL, sequences=NULL, ...){
    ## CHECK THAT REQUESTED INFOR IS THERE ##
    if(is.null(x@individuals)) individuals <- NULL
    if(is.null(x@samples)) {
        samples <- date.from <- date.to <- NULL
        locus <- sequences <- NULL
    } else {
        if(is.null(x@samples$locus)) locus <- NULL
    }
    if(is.null(x@dna)){
        locus <- sequences <- NULL
    }


    ## SUBSET BY INDIVIDUALS ##
    if(!is.null(individuals)){
        ## process logical/integers/numeric
        if(is.logical(individuals) || is.integer(individuals) || is.numeric(individuals)){
            individuals <- get.individuals(x)[individuals]
        }

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
        ## process logical/integers/numeric
        if(is.logical(samples) || is.integer(samples) || is.numeric(samples)){
            samples <- get.samples(x)[samples]
        }

        ## check that indicated samples are known
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


    ## SUBSET BY DATES ##
    ## dates from ... ##
    if(!is.null(date.from)){
        date.from <- as.Date(date.from, format=date.format)
        samples.tokeep <- x@samples$date >= date.from
        x <- subset(x, samples=samples.tokeep)
    }

    ## dates to ... ##
    if(!is.null(date.to)){
        date.to <- as.Date(date.to, format=date.format)
        samples.tokeep <- x@samples$date <= date.to
        x <- subset(x, samples=samples.tokeep)
    }


    ## SUBSET BY LOCUS ##
    if(!is.null(locus)){
        ## process logical/integers/numeric
        if(is.logical(locus) || is.integer(locus) || is.numeric(locus)){
            locus <- get.locus(x)[locus]
        }

        ## subset @samples
        x@samples <- x@samples[x@samples$locus %in% locus,,drop=FALSE]

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
    }


    ## SUBSET BY SEQUENCE ID ##
    if(!is.null(sequences)){
        ## process logical/integers/numeric
        if(is.logical(sequences) || is.integer(sequences) || is.numeric(sequences)){
            sequences <- get.sequences(x)[sequences]
        }

        ## subset @samples
        x@samples <- x@samples[x@samples$sequenceID %in% sequences,,drop=FALSE]

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

    }


    return(x)
}) # end obkData method




