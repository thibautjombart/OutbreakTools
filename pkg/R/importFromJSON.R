JSON2obkData <- function(individuals=NULL, records=NULL, contacts=NULL, context=NULL){
    ## AUXILIARY FUNCTIONS ##
    ## function to fill in vectors so that they all have 'allfields' entries
    f1 <- function(x){
        out <- as.character(rep(NA, length(allfields)))
        names(out) <- allfields
        out[names(x)] <- x
        return(out)
    }

    ## function removing all columns of a data.frame that are all NAs
    f2 <- function(x){
        toKeep <- sapply(x, function(e) !all(is.na(e)))
        out <- x[,toKeep,drop=FALSE]
        return(out)
    }


    ## INITIALIZE RESULTS ##
    tabind <- NULL
    listtabrec <- NULL
    tabcontacts <- NULL


    ## EXTRACT INDIVIDUAL DATA ##
    if(!is.null(individuals)){
        ## convert from json to list
        datind <- fromJSON(individuals)

        ## get data into a data.frame ##
        ## get all fields
        temp <- lapply(datind, unlist, recursive=TRUE)
        allfields <- unique(unlist(lapply(temp, names)))

        ## get data into a data.frame
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


    ## EXTRACT RECORDS DATA ##
    if(!is.null(records)){
        ## convert from json to list
        datrec <- fromJSON(records)

        ## get data into a data.frame ##
        ## get all fields
        temp <- lapply(datrec, unlist, recursive=TRUE)
        allfields <- unique(unlist(lapply(temp, names)))

        ## get data into a data.frame
        tabrec <- matrix(unlist(lapply(temp, f1)), nrow=length(datrec), byrow=TRUE)
        tabrec <- as.data.frame(tabrec)
        names(tabrec) <- allfields

        ## restore numerics where needed
        tabrec <- data.frame(lapply(tabrec, .restoreNumericType))

        ## restore dates were needed
        areDates <- grep("date", names(tabrec), ignore.case=TRUE)
        if(length(areDates)>1) for(i in areDates){
            tabrec[[i]] <- .process.Date(tabrec[[i]])
        }

        ## get the final list for @records ##
        ## get list
        temp <- grep("choice",names(tabrec), ignore.case=TRUE)
        listtabrec <- split(tabrec[-temp], tabrec[,temp])

        ## remove columns which are all NAs
        listtabrec <- lapply(listtabrec,f2)


        ## FILTER TABS FOR CONTACT INFORMATION ##
        contact.info <- grep("contact", names(listtabrec), ignore.case=TRUE)
        if(length(contact.info)>0){
            ## extract the right table ##
            tabcontacts <- listtabrec[[contact.info]]
            listtabrec <- listtabrec[-contact.info]

            ## create a from-to table ##

        }
    }


    ## TODO: TREAT contacts, context ##

    ## BUILD OBJECT AND RETURN ##
    out <- new("obkData", individuals=tabind, records=listtabrec)
    return(out)
}
