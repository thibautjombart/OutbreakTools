
########################
####  BASIC METHODS ####
########################


##########
## show ##
##########

setMethod("show", "obkData", function(object){
    N <- length(slotNames(object))
    cat("\n== obkData object ==")
    empty <- rep(TRUE, N)
    for(i in 1:N){
        if(!is.null(slot(object, slotNames(object)[i]))){
            cat(paste("\n@", slotNames(object)[i], "\n",sep=""))
            print(slot(object, slotNames(object)[i]))
            empty[i] <- FALSE
        }
    }

    if(any(empty)){
        txt <- paste("@", slotNames(object)[empty], collapse=", ", sep="")
        cat("\nEmpty slots:", txt)
    }

    cat("\n")
})


#############
## summary ##
#############
setMethod("summary", "obkData", function(object, ...){
    for (n in 1:6){
        print(c("Summary of ", slotNames(object)[n]),quote=FALSE)
	print(summary(slot(object,slotNames(object)[n])))
    }
    return()
})












