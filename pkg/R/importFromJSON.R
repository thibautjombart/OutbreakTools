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

        ## look for fields 'name', generate unique ID, if no field 'individualID' ##
        temp <- .retrieveLabelsFromDataframe(individuals.input)
        if(!"individualID" %in% names(individuals.input) && !is.null(temp)){
            ## assign labels
            row.names(individuals.input) <- individuals.input$individualID <- temp
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

            ## look for fields 'name', generate unique ID, if no field 'individualID' ##
            temp <- .retrieveLabelsFromDataframe(contacts.input)
            if(!"individualID" %in% names(contacts.input) && !is.null(temp)){
                ## assign labels
                contacts.input$to <- temp
            }

            ## match contact info with known individuals ##
            if(!is.null(individual.input)){
                fieldToMatch <- intersect(
                                          grep("key", names(individuals.input), ignore.case=TRUE, value=TRUE),
                                          grep("key", names(contacts.input), ignore.case=TRUE, value=TRUE)
                                          )
                from <- merge(individuals.input, contacts.input, by=fieldToMatch)$individualID
            } else stop("contact information provided without individual information")

            ## find if contacts are dated or not ##

            ## create a from-to table ##

        }
    }


    ## TODO: TREAT contacts, context ##

    ## BUILD OBJECT AND RETURN ##
    out <- new("obkData", individuals=individuals.input, records=records.input)
    return(out)
} # end JSON2obkData
