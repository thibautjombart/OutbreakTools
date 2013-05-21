
############################
####  CLASSE DEFINITION ####
############################

## CLASS DESCRIPTION:
## Instance of obkData store outbreak data; its content includes:
## - @individuals: meta-information on the individuals (group, etc.), stored as a data.frame
## - @samples: data about samples, stored as a data.frame
## - @clinical:list of clinical datasets, each stored as a data.frame information about interventions and events
## - @dna: dna data, stored as a obkSequences object
## - @contacts: contact information as obkContacts
setClass("obkData", representation(individuals="dataframeOrNULL", samples="dataframeOrNULL",
                                   clinical="listOrNULL", dna="obkSequencesOrNULL", contacts="obkContactsOrNULL",
                                   trees="multiPhyloOrNULL"),
         prototype(individuals=NULL, samples=NULL, dna=NULL, clinical=NULL, contacts=NULL, trees=NULL))







######################
####  CONSTRUCTOR ####
######################

## INPUT DESCRIPTION:
## 'individuals': a data.frame with any information on the individuals, each row being an individual, with the following columns:
## - "individualID"
## - any other named column
##
## 'samples': a data.frame where each row is an observation made on a sample, and the following mandatory columns:
## - "individualID"
## - "sampleID"
## - "date"
## - any optional, named column
## - "sequenceID": optional but particular processing by the constructor, a sequence ID existing in 'dna'
## - "locus": optional but particular processing by the constructor, the locus of a sequence
##
## 'dna': a DNAbin list with named sequences
##
## 'clinical': list of clinical datasets, each stored as a data.frame
##
## 'contacts': a matrix of characters indicating edges using two columns; if contacts are directed,
## the first column is 'from', the second is 'to'; values should match individual IDs (as returned
## by get.individuals(x)); if numeric values are provided, these are converted as integers and
## assumed to correspond to individuals returned by get.individuals(x).
##
## 'contacts.start': a vector of dates indicating the beginning of each contact
##
## 'contacts.end': a vector of dates indicating the end of each contact
##
## 'contacts.duration': another way to specify contacts.end, as duration of contact
##
##
setMethod("initialize", "obkData", function(.Object, individuals=NULL, samples=NULL, clinical=NULL, dna=NULL, trees=NULL,
                                            contacts=NULL, contacts.start=NULL, contacts.end=NULL, contacts.duration=NULL,
                                            contacts.directed=FALSE, date.format=NULL, ...){

    ## RETRIEVE PROTOTYPED OBJECT ##
    x <- .Object

    ## store old option ##
    o.opt <- options("stringsAsFactors")
    options("stringsAsFactors"=FALSE)
    on.exit(options(o.opt))

    ## escape of obkData is provided ##
    if(inherits(individuals, "obkData")) return(individuals)

    ## PROCESS PROVIDED INFORMATION ##
    ## coerce to data.frames, force to NULL if nrow=0
    if(!is.null(individuals)) {
        individuals <- as.data.frame(individuals)
                                        #    if(nrow(individuals)==0 || ncol(individuals)==1) individuals <- NULL
        if(nrow(individuals)==0 || ncol(individuals)==0) individuals <- NULL
    }
    if(!is.null(samples)){
        samples <- as.data.frame(samples)
        if(nrow(samples)==0) samples <- NULL
    }
    if(!is.null(clinical)) {
        if(is.data.frame(clinical))
            clinical <- list(clinical)
        else clinical <- as.list(clinical)
        if(length(clinical)==0) clinical <- NULL
    }
    if(!is.null(dna) && (inherits(dna, "DNAbin") && is.matrix(dna))) dna <- as.list(dna)
    if(!is.null(dna) && (!is.list(dna) || !inherits(dna, "DNAbin"))) stop("dna is not a list of DNAbin objects.")

    ## escape if no info provided ##
    if(is.null(individuals) && is.null(samples) && is.null(clinical) && is.null(dna) && is.null(contacts)) return(x)

    ## check that relevant fields are here ##
    if(!is.null(individuals)){
        if(!"individualID" %in% names(individuals) && is.null(row.names(individuals)))
          stop("no field 'individualID' in the individuals data.frame ('individuals')")
    }
    if(!is.null(samples)){
        if(!"individualID" %in% names(samples)) stop("no field 'individualID' in the sample data.frame ('samples')")
        if(!"sampleID" %in% names(samples)) stop("no field 'sampleID' in the sample data.frame ('samples')")
        if(!"date" %in% names(samples)) stop("no field 'date' in the sample data.frame ('samples')")
    }
    for(i in 1:length(clinical)){
        if(!is.null(clinical[[i]])){
            if(!"individualID" %in% names(clinical[[i]])) stop(paste("no field 'individualID' in the clinical data.frame", names(clinical)[i], ")"))
            if(!"date" %in% names(clinical[[i]])) stop(paste("no field 'date' in the clinical data.frame", names(clinical)[i], ")"))
        }
    }
    ## PROCESS INFORMATION ABOUT INDIVIDUALS ('individuals') ##
    if(!is.null(individuals)){
      if("individualID" %in% names(individuals))
        lab <- as.character(individuals[,"individualID"])
      else
        lab <- as.character(row.names(individuals))

        x@individuals <- individuals[, names(individuals)!="individualID", drop=FALSE]
        row.names(x@individuals) <- lab
    }


    ## PROCESS INFORMATION ABOUT SAMPLES ('samples') ##
    if(!is.null(samples)){
        ## reorder columns - mandatory fields come first
        nameOrder <- c(c("individualID","sampleID","date"), setdiff(names(samples), c("individualID","sampleID","date")))
        x@samples <- samples[,nameOrder]
        x@samples[,"individualID"] <- as.character(x@samples[,"individualID"])
        x@samples[,"sampleID"] <- as.character(x@samples[,"sampleID"])
        if(is.factor(x@samples[,"date"])) x@samples[,"date"] <- as.character(x@samples[,"date"])
        x@samples[,"date"] <- .process.Date(x@samples[,"date"], format=date.format)

        ## make sure that all individualIDs are in 'individuals', if the slot is not NULL
        if(!is.null(x@individuals)){
            unknownIDs <- unique(x@samples$individualID)[!unique(x@samples$individualID) %in% row.names(x@individuals)]
            if(length(unknownIDs)>0) {
                unknownIDs.txt <- paste(unknownIDs, collapse=", ")
                warning(paste("the following sampled individuals have no individual information:\n", unknownIDs.txt))
            }
        }
    }

    ## PROCESS INFORMATION ABOUT CLINICAL EVENTS ('clinicals') ##
    if(!is.null(clinical)){
        x@clinical <- list()

        ##if clinical is a data-frame (one set of clinical data)
        ##put it as a list
        if(is.data.frame(clinical))
          clinical=list(clinical)

        ## reorder the columns within each data frame.
        all.clinical.ID <- NULL
        for(i in 1:length(clinical))
        {
            nameOrder <- c(c("individualID","date"), setdiff(names(clinical[[i]]), c("individualID","date")))
            x@clinical[[i]] <- clinical[[i]][, nameOrder]
            x@clinical[[i]][,"individualID"] <- as.character(x@clinical[[i]][,"individualID"])
            if(is.factor(x@clinical[[i]][,"date"])) x@clinical[[i]][,"date"] <- as.character(x@clinical[[i]][,"date"])
            x@clinical[[i]][,"date"] <- .process.Date(x@clinical[[i]][,"date"], format=date.format)

            all.clinical.ID <- c(all.clinical.ID, x@clinical[[i]][, "individualID"])
        }

        names(x@clinical) <- names(clinical)

        ## make sure that all the individualIDs are in 'individuals', if the slot is not NULL
        if(!is.null(x@individuals)){
            unknownIDs <- unique(all.clinical.ID)[!unique(all.clinical.ID) %in% row.names(x@individuals)]
            if(length(unknownIDs)>0) {
                unknownIDs.txt <- paste(unknownIDs, collapse = ", ")
                warning(paste("the following individuals with clinical observations have no individual information:\n", unknownIDs.txt))
            }
        }
    }

    ## PROCESS INFORMATION ABOUT CONTACTS ('contacts') ##
    ## need to make sure that contact input is consisten with constructor
    if(!is.null(contacts)){
        ## process vertices indicated as numbers
        if(is.numeric(contacts)){
            if(!is.null(x@individuals)){
                ## replace with labels if available
                contacts <- matrix(row.names(x@individuals)[contacts], ncol=2)
            } else {
                ## convert as characters otherwise
                contacts <- matrix(as.character(contacts), ncol=2)
            }
        }

        ## check that all IDs match @individuals
        if(!is.null(x@individuals)){
            unknownIDs <- unique(contacts)[!unique(contacts) %in% row.names(x@individuals)]
            if(length(unknownIDs)>0) {
                unknownIDs.txt <- paste(unknownIDs, collapse = ", ")
                warning(paste("the following individuals with contact matrix have no individual information:\n", unknownIDs.txt))
            }
        }
        ## pass arguments to the obkContacts constructor
        x@contacts <- new("obkContacts", contactFrom=contacts[,1,drop=TRUE], contactTo=contacts[,2,drop=TRUE],
                          directed=contacts.directed, contactStart=contacts.start, contactEnd=contacts.end,
                          duration=contacts.duration)
    }


    ## PROCESS INFORMATION ABOUT DNA SEQUENCES ('sequenceID') ##
    if(!is.null(dna)){ # if DNA provided
        ## match labels with sample info
        if(!is.null(x@samples)){

            ## identify NAs
            isNA <- is.na(samples$sequenceID)

            ## check unknown labels
            if(is.character(samples$sequenceID) && !all(samples$sequenceID[!isNA] %in% names(dna))) {
                err.txt <- na.omit(samples$sequenceID[!samples$sequenceID %in% names(dna)])
                err.txt <- paste(unique(err.txt), collapse=", ")
                stop(paste("The following sequence ID were not found in the dna list:\n", err.txt))
            }

            ## pass information to constructor
            x@dna <- new("obkSequences", dna[x@samples$sequenceID[!isNA]], x@samples$locus[!isNA])

            ## set labels in @samples
            if(is.integer(x@samples$sequenceID) || is.numeric(x@samples$sequenceID)) x@samples$sequenceID[!isNA] <- names(dna)[x@samples$sequenceID[!isNA]]
        } else {
            ## warning
            warning("DNA sequences provided without sample information - no label matching, and assuming a single locus")
            ## pass information to constructor
            x@dna <- new("obkSequences", dna)
        }
    } else { # if no DNA
        x@dna <- NULL
    }


    ## PROCESS INFORMATION ABOUT PHYLOGENIES ('trees') ##
    if(!is.null(trees)){
        ## check class
        if(!inherits(trees, "multiPhylo")) stop("trees must be a multiPhylo object")

        ## check label consistency (to be added)
        x@trees <- trees
    }


    ## RETURN OBJECT ##
    return(x)
}) # end obkData constructor








####################
####  ACCESSORS ####
####################






##################
####  TESTING ####
##################
## NOTE: THIS MUST BE COMMENTED WHEN COMPILING/INSTALLING THE PACKAGE

## ## EMPTY OBJECT ##
## new("obkData")

## ## INDIVIDUAL INFO ONLY ##
## new("obkData", individuals=data.frame("individualID"=letters))
## new("obkData", individuals=data.frame("individualID"=letters, age=1:26, 1:26))


## samp <- data.frame(individualID=c('toto','toto','titi'), sampleID=c(1,3,2), date=c("2001-02-13","2001-03-01","2001-05-25"), swab=c("+","-","+"))


## ## SAMPLE INFO ONLY ##
## new("obkData", sample=samp)
## new("obkData", sample=samp[,c(1:3)] )
## new("obkData", sample=samp[,c(1:3,4,4,4)] )

## ## SAMPLE & INDIV INFO - MISSING INDIV ##
## new("obkData", sample=samp[,c(1:3,4,4,4)] , individuals=data.frame("individualID"=letters, age=1:26))

## ## SAMPLE & INDIV INFO ##
## ind <- data.frame("individualID"=c("toto","John Doe", "titi"), age=c(20,18,67), sex=c("m","m","?"))
## new("obkData", sample=samp, ind=ind)


## ## DNA INFO, NOTHING ELSE ##
## library(ape)
## data(woodmouse)
## dat.dna <- as.list(woodmouse)

## new("obkData", dna=dat.dna) # should be empty

## ## SAMP + DNA INFO ##
## samp <- data.frame(individualID=c('toto','toto','titi'), sampleID=c(1,3,2), date=c("2001-02-13","2001-03-01","2001-05-25"), swab=c("+","-","+"))

## samp <- cbind.data.frame(samp, sequenceID=c(1,2,3))

## ## sequences given as indices
## new("obkData", samples=samp, dna=dat.dna) # (note the nice sample ordering)

## ## sequences given as IDs
## samp$sequenceID <- c("No304","No306","No305")
## new("obkData", samples=samp, dna=dat.dna) # (note the nice sample ordering)

## ## sequences given as IDs, with wrong IDs
## samp$sequenceID <- c("No304","No306","Arrrhhh")
## new("obkData", samples=samp, dna=dat.dna) # (note the nice sample ordering)


## ## multiple sequences per individual
## samp$sequenceID <- c("No304","No306","No305")
## samp <- samp[c(1,1,2,2,2,3),]
## samp$sequenceID <- 1:6
## new("obkData", samples=samp, dna=dat.dna)


## ## multiple sequences per individual, locus information
## samp$locus <- c("gene1","gene2")[c(1,1,1,2,1,2)]
## new("obkData", samples=samp, dna=dat.dna)

# ## ## clinical data
# data(FakeInfluenza)
# inds <- data.frame(individualID = c("Lulla", "Paul"), gender = c("F", "M"))
# inds2 <- data.frame(gender = c("F", "M"))
# row.names(inds2) <- c("Lulla", "Paul")
# x <- new("obkData", individuals =  FakeInfluenza$Patients, clinical = FakeInfluenza$Clinical, date.format = "%d/%m/%Y")
# x <- new("obkData", individuals = inds, clinical = FakeInfluenza$Clinical, date.format = "%d/%m/%Y") ## should give a warning that an individual record for Anne is missing
# x <- new("obkData", individuals = inds2, date.format = "%d/%m/%Y") ## should give a warning that an individual record for Anne is missing
# x <- new("obkData", individuals =  FakeInfluenza$Patients, sample=FakeInfluenza$Samples, clinical = FakeInfluenza$Clinical, date.format = "%d/%m/%Y") ## adding the sample part

#
# get.nclinicals(x)
# get.clinicals(x)
# get.individuals(x,"clinical")
# get.nindividuals(x,"clinical")
# get.dates(x,"clinical")
