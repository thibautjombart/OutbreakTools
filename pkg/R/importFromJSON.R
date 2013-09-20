JSON2obkData <- function(individuals=NULL, records=NULL, contacts=NULL, context=NULL){
    ## EXTRACT INDIVIDUAL DATA ##
    if(!is.null(individuals)){
        ## convert from json to list
        datind <- fromJSON(individuals)

        ## get data into a data.frame ##
        ## get all fields
        temp <- lapply(datind, unlist, recursive=TRUE)
        allfields <- unique(unlist(lapply(temp, names)))

        ## function to fill in vectors so that they all have 'allfields' entries
        f1 <- function(x){
            out <- as.character(rep(NA, length(allfields)))
            names(out) <- allfields
            out[names(x)] <- x
            return(out)
        }

        tabind <- matrix(unlist(lapply(temp, f1)), nrow=length(datind), byrow=TRUE)
        tabind <- as.data.frame(tabind)
        names(tabind) <- allfields

        ## restore numerics where needed
        tabind <- data.frame(lapply(tabind, .restoreNumericType))

        ## restore dates were needed
        areDates <- grep("date", names(tabind), ignore.case=TRUE)
        if(length(areDates)>1) for(i in areDates){
            tabind[[i]] <- .process.Date(tabind[[i]])
        }
    }


    ## TODO: TREAT records, contacts, context ##

    ## BUILD OBJECT AND RETURN ##
    out <- new("obkData", individuals=tabind)
    return(out)
}
