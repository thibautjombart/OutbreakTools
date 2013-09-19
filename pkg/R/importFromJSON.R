JSON2obkData <- function(individuals=NULL, records=NULL, contacts=NULL, context=NULL){
    ## EXTRACT INDIVIDUAL DATA ##
    if(!is.null(individuals)){
        ## convert from json to list
        datind <- fromJSON(individuals)

        ## get data into a data.frame
        tabind <- Reduce(rbind.data.frame, lapply(datind, unlist, recursive=TRUE))
        if(!is.data.frame(tabind)){ # case of 1 single row
            tabind <- data.frame(t(tabind))
        }

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
