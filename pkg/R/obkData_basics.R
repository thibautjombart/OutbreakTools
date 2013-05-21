
########################
####  BASIC METHODS ####
########################


##########
## show ##
##########

setMethod("show", "obkData", function(object){
    N <- length(slotNames(object))
    cat("\n=== obkData object ===")
    empty <- rep(TRUE, N)
    for(i in 1:N){
        if(!is.null(slot(object, slotNames(object)[i]))){
            cat(paste("\n== @", slotNames(object)[i], "== \n",sep=""))
            print(slot(object, slotNames(object)[i]))
            empty[i] <- FALSE
        }
    }

    if(any(empty)){
        txt <- paste("@", slotNames(object)[empty], collapse=", ", sep="")
        cat("\n== Empty slots == \n", txt)
    }

    cat("\n")
})



#############
## summary ##
#############

setMethod("summary", "obkData", function(object, ...){

    cat(paste("Dataset of ",get.nindividuals(object,"all")," individuals with...\n",sep=""))

    cat(paste("- ",get.nsamples(object)," samples \n\tcoming from ",get.nindividuals(object,"samples")," individuals",sep=""))

    if(!is.null(object@samples) && dim(object@samples)[2] <=13 )
    {
        cat(paste("\n\tcollected between ",min(object@samples$date)," and ",max(object@samples$date),"\n \tcontaining information on:\n",sep=""))
        for (i in (1:dim(object@samples)[2])[-which((names(object@samples) %in% c("individualID","sampleID","date"))==TRUE)])
        {
            cat(paste("\t\t ",names(object@samples)[i],"\t",sep=""))
            if(is.numeric(object@samples[,i])==TRUE){cat(paste("(mean: ",signif(mean((object@samples[,i]),na.rm=TRUE),digits=6),", sd: ",signif(sd((object@samples[,i]),na.rm=TRUE),digits=6),")",sep=""))}
            cat("\n")
        }
    }else{cat("\n")}

	if(!is.null(object@dna))
	{
		if(get.nlocus(object)>1)
		{
			cat(paste("- ",get.nsequences(object)," sequences across ",get.nlocus(object)," loci \n\t(length of concatenated alignment: ",sum(sapply(object@dna@dna,ncol))," nucleotides)\n",sep=""))
		}else
		{
			cat(paste("- ",get.nsequences(object)," sequences across ",get.nlocus(object)," locus \n\t(length of concatenated alignment: ",sum(sapply(object@dna@dna,ncol))," nucleotides)\n",sep=""))
		}
	}else
	{
		cat("- 0 sequences\n")
	}

    cat(paste("- clinical data from ",get.nindividuals(object,"clinical")," individuals",sep=""))
    if(!is.null(object@clinical) && length(object@clinical) <=10 )
    {
        cat("\n\t containing information on:\n")
        for (i in 1:length(object@clinical))
        {
            cat(paste("\t\t ",names(object@clinical)[i],"\n",sep=""))
        }
    }else{cat("\n")}

    if(!is.null(object@contacts))
    {
        cat(paste("- ",get.ncontacts(object), " contacts recorded between ", get.nindividuals(object,"contacts")," individuals\n",sep=""))
    }

    if(!is.null(object@trees))
    {
		if(length(object@trees)>1)
		{
        	cat(paste("- ",length(object@trees)," phylogenetic trees with ",length(object@trees[[1]]$tip.label)," tips\n",sep=""))
		}else
		{
			cat(paste("- ",length(object@trees)," phylogenetic tree with ",length(object@trees[[1]]$tip.label)," tips\n",sep=""))
		}
    }

    return(invisible())
})


# test:
# library(epibase)
# data(HorseFlu)
# summary(HorseFlu)
# summary(new("obkData"))







##########
## head ##
##########
setMethod("head", "obkData", function(x, n=4L, ...){
    Nslots <- length(slotNames(x))
    cat("\n=== obkData x ===")
    empty <- rep(TRUE, Nslots)
    for(i in 1:Nslots){
        if(!is.null(slot(x, slotNames(x)[i]))){ # if slot is not NULL
            cat(paste("\n== @", slotNames(x)[i], "== \n",sep=""))
            if(inherits(slot(x, slotNames(x)[i]), c("obkSequences","obkContacts","multiPhylo"))){ # special classes
                print(slot(x, slotNames(x)[i]))
            } else if(is.list(slot(x, slotNames(x)[i])) && !is.data.frame(slot(x, slotNames(x)[i]))){ # use custom 'head' for lists
                lapply(slot(x, slotNames(x)[i]), function(e) print(head(e, n=n, ...)))
            } else {
                print(head(slot(x, slotNames(x)[i]), n=n, ...))
            }
            empty[i] <- FALSE
        }
    }

    if(any(empty)){
        txt <- paste("@", slotNames(x)[empty], collapse=", ", sep="")
        cat("\n== Empty slots == \n", txt)
    }

    cat("\n")
})





##########
## tail ##
##########
setMethod("tail", "obkData", function(x, n=4L, ...){
    Nslots <- length(slotNames(x))
    cat("\n=== obkData x ===")
    empty <- rep(TRUE, Nslots)
    for(i in 1:Nslots){
        if(!is.null(slot(x, slotNames(x)[i]))){
            cat(paste("\n== @", slotNames(x)[i], "== \n",sep=""))
            if(inherits(slot(x, slotNames(x)[i]), c("obkSequences","obkContacts","multiPhylo"))){ # special classes
                print(slot(x, slotNames(x)[i]))
            } else if(is.list(slot(x, slotNames(x)[i])) && !is.data.frame(slot(x, slotNames(x)[i]))){ # use custom 'tail' for lists
                lapply(slot(x, slotNames(x)[i]), function(e) print(tail(e, n=n, ...)))
            } else {
                print(tail(slot(x, slotNames(x)[i]), n=n, ...))
            }
            empty[i] <- FALSE
        }
    }

    if(any(empty)){
        txt <- paste("@", slotNames(x)[empty], collapse=", ", sep="")
        cat("\n== Empty slots == \n", txt)
    }

    cat("\n")
})










