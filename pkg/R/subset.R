

###############################
#### SUBSETTING PROCEDURES ####
###############################

#########################
## obkSequences method ##
#########################
setMethod("subset", "obkSequences", function(x, sequences=NULL, locus=NULL, individuals=NULL,
                                        date.from=NULL, date.to=NULL, date.format=NULL, ...){
    ## ESCAPE IF NOTHING IN THE DATASET ##
    if(get.nsequences(x)==0) return(x)

    ## SUBSET BY SEQUENCE ID ##
    if(!is.null(sequences)){
        ## handle non-character argument ##
        if(!is.character(sequences)){
            sequences <- get.sequences(x)[sequences]
        }

        ## check that all sequences are known ##
        if(!all(sequences %in% get.sequences(x))){
            temp <- paste(sequences[!sequences %in% get.sequences(x)], collapse=", ")
            warning(paste("The following sequences were not found in the data:", temp))
            sequences <- sequences[sequences %in% get.sequences(x)]
        }

        ## subset x@dna ##
        x@dna <- get.dna(x, id=sequences)
        
        ## subset x@meta ##
        x@meta <- x@meta[sequences,,drop=FALSE]

    } # end subset by sequence ID


    ## SUBSET BY LOCUS ##
    if(!is.null(locus)){
        ## handle non-character argument ##
        if(!is.character(locus)){
            locus <- get.locus(x)[locus]
        }

        ## check that all loci are known ##
        if(!all(locus %in% get.locus(x))){
            temp <- paste(locus[!locus %in% get.locus(x)], collapse=", ")
            warning(paste("The following loci were not found in the data:", temp))
            locus <- locus[locus %in% get.locus(x)]
        }

        ## subset x@dna ##
        x@dna <- x@dna[locus]
        seq.tokeep <- unlist(lapply(x@dna, rownames))

        ## subset x@meta ##
        x@meta <- x@meta[seq.tokeep,,drop=FALSE]

    } # end subset by locus


     ## SUBSET BY INDIVIDUAL ID ##
    if(!is.null(individuals)){
        ## handle non-character argument ##
        if(!is.character(individuals)){
            individuals <- get.individuals(x)[individuals]
        }

        ## check that all individuals are known ##
        if(!all(individuals %in% get.individuals(x))){
            temp <- paste(individuals[!individuals %in% get.individuals(x)], collapse=", ")
            warning(paste("The following individuals were not found in the data:", temp))
            individuals <- individuals[individuals %in% get.individuals(x)]
        }

        ## find sequences to retain ##
        seq.tokeep <- rownames(x@meta)[x@meta$individualID %in% individuals]

        ## subset by sequences ##
        x <- subset(x, sequences=seq.tokeep)
    } # end subset by individual ID


    ## DATES FROM ... ##
    if(!is.null(date.from)){
        ## process date ##
        date.from <- .process.Date(date.from, format=date.format)

        ## find sequences to retain ##
        seq.tokeep <- x@meta$date >= date.from

        ## subset by sequences ##
        x <- subset(x, sequences=seq.tokeep)
    } # end subset by date.from


    ## DATES TO ... ##
    if(!is.null(date.to)){
         ## process date ##
        date.to <- .process.Date(date.to, format=date.format)

        ## find sequences to retain ##
        seq.tokeep <- x@meta$date <= date.to

        ## subset by sequences ##
        x <- subset(x, sequences=seq.tokeep)
    } # end subset by date.to


    return(x)
}) # end subset for obkSequences






####################
## obkData method ##
####################
setMethod("subset", "obkData", function(x, individuals=NULL, locus=NULL, sequences=NULL,
                                        date.from=NULL, date.to=NULL, date.format=NULL,
                                        row.individuals=NULL, row.samples=NULL,...){
    ## CHECK THAT REQUESTED INFO IS THERE ##
    if(is.null(x@individuals)) {
        individuals <- row.individuals <- NULL
    }

    if(is.null(x@dna)){
        locus <- sequences <- NULL
    }

    ## SUBSET BY ROWS IN @INDIVIDUALS ##
    if(!is.null(row.individuals)){
        ## subset @individuals
        x@individuals <- x@individuals[row.individuals, ,drop=FALSE]

        ## subset @dna
        if(!is.null(x@dna)) x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @records
        if(!is.null(x@records)){
            for(i in 1:length(x@records)){
                x@records[[i]] <- x@records[[i]][x@records[[i]]$"individualID" %in% rownames(x@individuals), ,drop=FALSE]
            }
        }

    } # end subset by row.individuals


    ## ## SUBSET BY ROWS IN @SAMPLES ##
    ## if(!is.null(row.samples)){
    ##     ## subset @samples
    ##     if(!is.null(x@samples)) x@samples <- x@samples[row.samples, ,drop=FALSE]

    ##     ## subset @individuals
    ##    if(!is.null(x@individuals))  x@individuals <-x@individuals[rownames(x@individuals) %in% x@samples$individualID,,drop=FALSE]

    ##     ## subset @dna
    ##     if(!is.null(x@dna)) x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

    ##     ## subset @records
    ##     if(!is.null(x@records)){
    ##         for(i in 1:length(x@records)){
    ##             x@records[[i]] <- x@records[[i]][x@records[[i]]$"individualID" %in% x@samples$"individualID", ,drop=FALSE]
    ##         }
    ##     }

    ## } # end subset by row in samples


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

        ## ## subset @dna !! TO CHANGE
        ## if(!is.null(x@dna)) x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @records
        if(!is.null(x@records)){
            for(i in 1:length(x@records)){
                x@records[[i]] <- x@records[[i]][x@records[[i]]$"individualID" %in% rownames(x@individuals), ,drop=FALSE]
            }
        }

    } # end processing 'individuals' argument


    ## SUBSET BY DATES ## !! TO CHANGE !!
    ## dates from ... ##
    ## if(!is.null(date.from)){
    ##     date.from <- .process.Date(date.from, format=date.format)
    ##     samples.tokeep <- x@samples$date >= date.from
    ##     x <- subset(x, row.samples=samples.tokeep)
    ##     if(!is.null(x@contacts) && inherits(x@contacts@contacts, "networkDynamic")){
    ##         x@contacts@contacts <- network.extract(x@contacts@contacts, onset=date.from)
    ##     }
    ## }

    ## ## dates to ... ##
    ## if(!is.null(date.to)){
    ##     date.to <- .process.Date(date.to, format=date.format)
    ##     samples.tokeep <- x@samples$date <= date.to
    ##     x <- subset(x, row.samples=samples.tokeep)
    ##     if(!is.null(x@contacts) && inherits(x@contacts@contacts, "networkDynamic")){
    ##         x@contacts@contacts <- network.extract(x@contacts@contacts, terminus=date.to)
    ##     }
    ## }


    ## SUBSET BY LOCUS ##
    if(!is.null(locus)){
        ## process logical/integers/numeric
        if(is.logical(locus) || is.integer(locus) || is.numeric(locus)){
            locus <- get.locus(x)[locus]
        }

        ## ## subset @individuals # !! to change !!
        ## if(!is.null(x@individuals)) x@individuals <-x@individuals[rownames(x@individuals) %in% x@samples$individualID,,drop=FALSE]

        ## ## subset @dna # !! to change !!
        ## if(!is.null(x@dna)) x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## subset @records # !! to change !!
        ## if(!is.null(x@records)){
        ##     for(i in 1:length(x@records)){
        ##         x@records[[i]] <- x@records[[i]][x@records[[i]]$"individualID" %in% x@samples$"individualID", ,drop=FALSE]
        ##     }
        ## }

    }


    ## SUBSET BY SEQUENCE ID ##
    if(!is.null(sequences)){
        ## process logical/integers/numeric
        if(is.logical(sequences) || is.integer(sequences) || is.numeric(sequences)){
            sequences <- get.sequences(x)[sequences]
        }

        ## subset @individuals
        if(!is.null(x@individuals)) x@individuals <-x@individuals[rownames(x@individuals) %in% x@samples$individualID,,drop=FALSE]

        ## ## subset @dna # !! to change !!
        ## if(!is.null(x@dna)) x@dna@dna <- get.dna(x@dna, id=x@samples$sequenceID)

        ## ## subset @records # !! to change !!
        ## if(!is.null(x@records)){
        ##     for(i in 1:length(x@records)){
        ##         x@records[[i]] <- x@records[[i]][x@records[[i]]$"individualID" %in% x@samples$"individualID", ,drop=FALSE]
        ##     }
        ## }

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




