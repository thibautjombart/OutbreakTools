
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
#setMethod("summary", "obkData", function(object, ...){
#    for (n in 1:6){
#        print(c("Summary of ", slotNames(object)[n]),quote=FALSE)
#	print(summary(slot(object,slotNames(object)[n])))
#    }
#    return()
#})

setMethod("summary", "obkData", function(object, ...){
	#get.nindividuals(object,"all")
	cat(paste("Dataset of ",get.nindividuals(object,"individuals")," individuals\n",sep=""))
	cat(paste("with sample data for ",get.nindividuals(object,"samples")," individuals\n",sep=""))
	cat(paste("and clinical data for ",get.nindividuals(object,"clinical")," individuals\n",sep=""))
	#get.nclinicals(object)
	#get.nsamples(object)
	#get.nsequences(object)
	#get.nlocus(object)		
	return(invisible())
})

# test: 
# library(epibase)
# example(obkData)
# summary(x)










