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

    ## function to make new unique labels from fields matching 'name' in a data.frame
    f3 <- function(x, sep=" "){
        ## look for fields 'name', generate unique ID ##
        temp <- grep("name", names(x), ignore.case=TRUE)
        if(length(temp)>0){
            name.fields <- names(x)[temp]

            ## check order ##
            ## 'first' name first
            temp <- grep("first", name.fields, ignore.case=TRUE)
            if(length(temp)>0) name.fields <- c(name.fields[temp],name.fields[-temp])

            ## 'last' or 'sur' names last
            temp <- grep("last", name.fields, ignore.case=TRUE)
            if(length(temp)>0) name.fields <- c(name.fields[-temp],name.fields[temp])
            temp <- grep("sur", name.fields, ignore.case=TRUE)
            if(length(temp)>0) name.fields <- c(name.fields[-temp],name.fields[temp])

            ## generate new unique names ##
            out <- apply(x[,name.fields],1,paste, collapse=sep)
            out <- make.unique(out)
            return(out)
        } else
        return(NULL)
    }


    ## INITIALIZE RESULTS ##
    individuals.input <- NULL
    records.input <- NULL
    contacts.input <- NULL


    ## EXTRACT INDIVIDUAL DATA ##
    if(!is.null(individuals)){
        ## convert from json to list
        datind <- fromJSON(individuals)

        ## get data into a data.frame ##
        ## get all fields
        temp <- lapply(datind, unlist, recursive=TRUE)
        allfields <- unique(unlist(lapply(temp, names)))

        ## get data into a data.frame
        individuals.input <- matrix(unlist(lapply(temp, f1)), nrow=length(datind), byrow=TRUE)
        individuals.input <- as.data.frame(individuals.input)
        names(individuals.input) <- allfields

        ## restore numerics where needed
        individuals.input <- data.frame(lapply(individuals.input, .restoreNumericType))

        ## restore dates were needed
        areDates <- grep("date", names(individuals.input), ignore.case=TRUE)
        if(length(areDates)>1) for(i in areDates){
            individuals.input[[i]] <- .process.Date(individuals.input[[i]])
        }

        ## look for fields 'name', generate unique ID ##
        temp <- grep("name",names(individuals.input))
        if(length(temp)>0 && !"individualID" %in% names(individuals.input)){
            ## make new labels
            newnames <- apply(individuals.input[,temp],1,paste, collapse=" ")

            ## make sure they're all unique
            newnames <- make.unique(newnames)

            ## assign labels
            row.names(individuals.input) <- individuals.input$individualID <- newnames
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
        records.input <- split(tabrec[-temp], tabrec[,temp])

        ## remove columns which are all NAs
        records.input <- lapply(records.input,f2)


        ## FILTER TABS FOR CONTACT INFORMATION ##
        contact.info <- grep("contact", names(records.input), ignore.case=TRUE)
        if(length(contact.info)>0){
            ## extract the right table ##
            contacts.input <- records.input[[contact.info]]
            records.input <- records.input[-contact.info]

            ## match contact info with known individuals ##
            if(!is.null(individual.input)){

            }
            ## find if contacts are dated or not ##

            ## create a from-to table ##

        }
    }


    ## TODO: TREAT contacts, context ##

    ## BUILD OBJECT AND RETURN ##
    out <- new("obkData", individuals=individuals.input, records=records.input)
    return(out)
} # end JSON2obkData
