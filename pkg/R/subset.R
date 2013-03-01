

###############################
#### SUBSETTING PROCEDURES ####
###############################

###################
## obkData method ##
###################
setMethod("subset", "obkData", function(x, individuals=NULL, samples=NULL, locus=NULL, sequences=NULL,
                                        date.from=NULL, date.to=NULL, date.format="",
                                        row.individuals=NULL, row.samples=NULL,...){
    ## CHECK THAT REQUESTED INFOR IS THERE ##
    if(is.null(x@individuals)) {
        individuals <- row.individuals <- NULL
    }
    if(is.null(x@samples)) {
        samples <- date.from <- date.to <- row.samples <- NULL
        locus <- sequences <- NULL
    } else {
        if(is.null(x@samples$locus)) locus <- NULL
    }
    if(is.null(x@dna)){
        locus <- sequences <- NULL
    }

    ## SUBSET BY ROWS IN @INDIVIDUALS ##
    if(!is.null(row.individuals)){
        ## check that subset info has right length
        if(length(row.individuals)!=nrow(x@individuals)){
            stop(paste("row.individuals has a wrong length (provided: ",
                       length(row.individuals), " expected: ", nrow(x@individuals),")",sep=""))
        }

        ## subset @individuals
        x@individuals <- x@individuals[row.individuals, ,drop=FALSE]

        ## subset @samples
        x@samples <- x@samples[x@samples$individualID %in% rownames(x@individuals), ,drop=FALSE]

        ## subset @dna
        x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @contacts
        ## (TODO)

        ## subset @clinicals
        ## (TODO)
    } # end subset by row.individuals


    ## SUBSET BY ROWS IN @SAMPLES ##
    if(!is.null(row.samples)){
        ## check that subset info has right length
        if(length(row.samples)!=nrow(x@samples)){
            stop(paste("row.samples has a wrong length (provided: ",
                       length(row.samples), " expected: ", nrow(x@samples),")",sep=""))
        }

        ## subset @samples
        x@samples <- x@samples[row.samples, ,drop=FALSE]

        ## subset @individuals
        x@individuals <-x@individuals[rownames(x@individuals) %in% x@samples$individualID,,drop=FALSE]

        ## subset @dna
        x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @contacts
        ## (TODO)

        ## subset @clinicals
        ## (TODO)
    } # end subset by row.individuals


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

    }

    ## SUBSET @TREES ##
    ## these only depend on @sample, so only one code is useful here
    if(!is.null(x@trees)){
        for(i in 1:length(x@trees)){
            toDrop <- x@trees[[i]]$tip.label[! x@trees[[i]]$tip.label %in% get.sequences(x)]
            x@trees[[i]] <- drop.tip(x@trees[[i]], tip=toDrop, trim.internal=TRUE, subtree=FALSE)
        }
    }

    return(x)
}) # end obkData method




