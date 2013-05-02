

###############################
#### SUBSETTING PROCEDURES ####
###############################

###################
## obkData method ##
###################
setMethod("subset", "obkData", function(x, individuals=NULL, samples=NULL, locus=NULL, sequences=NULL,
                                        date.from=NULL, date.to=NULL, date.format=NULL,
                                        row.individuals=NULL, row.samples=NULL,...){
    ## CHECK THAT REQUESTED INFO IS THERE ##
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
        ## subset @individuals
        x@individuals <- x@individuals[row.individuals, ,drop=FALSE]

        ## subset @samples
        if(!is.null(x@samples)) x@samples <- x@samples[x@samples$individualID %in% rownames(x@individuals), ,drop=FALSE]

        ## subset @dna
        if(!is.null(x@dna)) x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @clinical
        if(!is.null(x@clinical)){
            for(i in 1:length(x@clinical)){
                x@clinical[[i]] <- x@clinical[[i]][x@clinical[[i]]$"individualID" %in% rownames(x@individuals), ,drop=FALSE]
            }
        }

    } # end subset by row.individuals


    ## SUBSET BY ROWS IN @SAMPLES ##
    if(!is.null(row.samples)){
        ## subset @samples
        if(!is.null(x@samples)) x@samples <- x@samples[row.samples, ,drop=FALSE]

        ## subset @individuals
       if(!is.null(x@individuals))  x@individuals <-x@individuals[rownames(x@individuals) %in% x@samples$individualID,,drop=FALSE]

        ## subset @dna
        if(!is.null(x@dna)) x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @clinical
        if(!is.null(x@clinical)){
            for(i in 1:length(x@clinical)){
                x@clinical[[i]] <- x@clinical[[i]][x@clinical[[i]]$"individualID" %in% x@samples$"individualID", ,drop=FALSE]
            }
        }

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
        if(!is.null(x@individuals)) x@individuals <- x@individuals[individuals, ,drop=FALSE]

        ## subset @samples
        if(!is.null(x@samples)) x@samples <- x@samples[x@samples$individualID %in% individuals, ,drop=FALSE]

        ## subset @dna
        if(!is.null(x@dna)) x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @clinical
        if(!is.null(x@clinical)){
            for(i in 1:length(x@clinical)){
                x@clinical[[i]] <- x@clinical[[i]][x@clinical[[i]]$"individualID" %in% rownames(x@individuals), ,drop=FALSE]
            }
        }

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
        if(!is.null(x@samples)) x@samples <-x@samples[x@samples$sampleID %in% samples, ,drop=FALSE]

        ## subset @individuals
       if(!is.null(x@individuals))  x@individuals <-x@individuals[rownames(x@individuals) %in% x@samples$individualID,,drop=FALSE]

        ## subset @dna
       if(!is.null(x@dna))  x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @clinical
        if(!is.null(x@clinical)){
            for(i in 1:length(x@clinical)){
                x@clinical[[i]] <- x@clinical[[i]][x@clinical[[i]]$"individualID" %in% x@samples$"individualID", ,drop=FALSE]
            }
        }

    } # end processing 'samples' argument


    ## SUBSET BY DATES ##
    ## dates from ... ##
    if(!is.null(date.from)){
        date.from <- .process.Date(date.from, format=date.format)
        samples.tokeep <- x@samples$date >= date.from
        x <- subset(x, row.samples=samples.tokeep)
        if(!is.null(x@contacts) && inherits(x@contacts@contacts, "networkDynamic")){
            x@contacts@contacts <- network.extract(x@contacts@contacts, onset=date.from)
        }
    }

    ## dates to ... ##
    if(!is.null(date.to)){
        date.to <- .process.Date(date.to, format=date.format)
        samples.tokeep <- x@samples$date <= date.to
        x <- subset(x, row.samples=samples.tokeep)
        if(!is.null(x@contacts) && inherits(x@contacts@contacts, "networkDynamic")){
            x@contacts@contacts <- network.extract(x@contacts@contacts, terminus=date.to)
        }
    }


    ## SUBSET BY LOCUS ##
    if(!is.null(locus)){
        ## process logical/integers/numeric
        if(is.logical(locus) || is.integer(locus) || is.numeric(locus)){
            locus <- get.locus(x)[locus]
        }

        ## subset @samples
        if(!is.null(x@samples)) x@samples <- x@samples[x@samples$locus %in% locus,,drop=FALSE]

        ## subset @individuals
        if(!is.null(x@individuals)) x@individuals <-x@individuals[rownames(x@individuals) %in% x@samples$individualID,,drop=FALSE]

        ## subset @dna
        if(!is.null(x@dna)) x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @clinical
        if(!is.null(x@clinical)){
            for(i in 1:length(x@clinical)){
                x@clinical[[i]] <- x@clinical[[i]][x@clinical[[i]]$"individualID" %in% x@samples$"individualID", ,drop=FALSE]
            }
        }

    }


    ## SUBSET BY SEQUENCE ID ##
    if(!is.null(sequences)){
        ## process logical/integers/numeric
        if(is.logical(sequences) || is.integer(sequences) || is.numeric(sequences)){
            sequences <- get.sequences(x)[sequences]
        }

        ## subset @samples
        if(!is.null(x@samples)) x@samples <- x@samples[x@samples$sequenceID %in% sequences,,drop=FALSE]

        ## subset @individuals
        if(!is.null(x@individuals)) x@individuals <-x@individuals[rownames(x@individuals) %in% x@samples$individualID,,drop=FALSE]

        ## subset @dna
        if(!is.null(x@dna)) x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @clinical
        if(!is.null(x@clinical)){
            for(i in 1:length(x@clinical)){
                x@clinical[[i]] <- x@clinical[[i]][x@clinical[[i]]$"individualID" %in% x@samples$"individualID", ,drop=FALSE]
            }
        }

    }


    ## SUBSET CONTACTS
    ## subset @contacts - static or dynamic network
    if(!is.null(x@contacts)){
        toRemove <- which(!network.vertex.names(x@contacts@contacts) %in%  rownames(x@individuals)) # individuals to remove
        x@contacts@contacts <- delete.vertices(x@contacts@contacts, toRemove) # remove vertices
    }

    ## SUBSET @TREES ##
    ## these only depend on @sample, so only one code is useful here
    if(!is.null(x@trees)){
        for(i in 1:length(x@trees)){
            toDrop <- x@trees[[i]]$tip.label[! x@trees[[i]]$tip.label %in% get.sequences(x)]
            ## check that we don't try to have a tree with one tip - creates an error
            if(length(setdiff(x@trees[[i]]$tip.label, toDrop))==1){
                x@trees <- x@trees[-i]
            } else {
                x@trees[[i]] <- drop.tip(x@trees[[i]], tip=toDrop, trim.internal=TRUE, subtree=FALSE)
            }
        }
    }

    return(x)
}) # end obkData method




