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
    contacts.input <- fromto <- date.start <- date.end <- NULL


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
    } # end individuals info


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
        for(i in areDates){
            tabrec[,i] <- .process.Date(tabrec[,i])
        }

        ## get the final list for @records ##
        ## get list
        temp <- grep("choice",names(tabrec), ignore.case=TRUE)
        records.input <- split(tabrec[-temp], tabrec[,temp])

        ## remove columns which are all NAs
        records.input <- lapply(records.input,f2)


        ## create clever labels and assign individualID  / restore dates ##
        if(!is.null(individuals.input)){
            for(i in 1:length(records.input)){
                ## labels ##
                fieldToMatch <- intersect(
                                          grep("key", names(individuals.input), ignore.case=TRUE, value=TRUE),
                                          grep("key", names(records.input[[i]]), ignore.case=TRUE, value=TRUE)
                                          )
                temp <- merge(records.input[[i]], individuals.input, by=fieldToMatch, all.x=TRUE, all.y=FALSE)$individualID
                records.input[[i]]$individualID <- temp

                ## dates ##
                areDates <- grep("date", names(records.input[[i]]), ignore.case=TRUE)
                if(length(areDates)>1) for(j in areDates){
                    records.input[[i]][[j]] <- .process.Date( records.input[[i]][[j]])
                }
            } # end labels/dates stuff
        }

        ## FILTER TABS FOR CONTACT INFORMATION ##
        contact.info <- grep("contact", names(records.input), ignore.case=TRUE)
        if(length(contact.info)>0){
            ## extract the right table ##
            contacts.input <- records.input[[contact.info]]
            records.input <- records.input[-contact.info]

            ## look for fields 'name', generate unique ID, if no field 'individualID' ##
            temp <- .retrieveLabelsFromDataframe(contacts.input, unique=FALSE)
            if("individualID" %in% names(contacts.input) && !is.null(temp)){
                ## assign labels
                from <- contacts.input$individualID
                to <- temp
                fromto <- data.frame(from, to)
            } else {
                warning("contact information provided without individual information, or without proper names of contacts - ignoring.")
                fromto <- NULL
            }

            ## find if contacts are dated or not ##
            areDates <- sapply(contacts.input, inherits, "Date")
            if(any(areDates)){
                ## seek starting date ##
                date.start <- unique(unlist(lapply(c("first","start","begin","initial","from"),
                                                 function(txt) grep(txt, names(contacts.input)[areDates], ignore.case=TRUE, value=TRUE))
                                          ))[1]
                date.start <- contacts.input[,date.start]

                ## seek ending date ##
                date.end <- unique(unlist(lapply(c("last","end","until","final","to"),
                                                 function(txt) grep(txt, names(contacts.input)[areDates], ignore.case=TRUE, value=TRUE))
                                          ))[1]
                date.end <- contacts.input[,date.end]
            } # end dates for contacts
        } # end contact info


        ## SORT OUT THE DATE FIELD ##
        if(!is.null(records.input)){
            for(i in 1:length(records.input)){
                if(!"date" %in% names(records.input[[i]])){
                    areDates <- sapply(records.input[[i]], inherits, "Date")
                    names(records.input[[i]])[areDates][1] <- "date"
                }
            }
        } # end sort out 'date' field in records

    } # end records info


    ## TODO: TREAT context ##


    ## CLEAN USELESS FIELDS ##
    ## fields to remove
    hidden.fields <- c("_entries","[.]accuracy","[.]bearing","[.]provider", "^id$",
                       "^created$","^DeviceID$", "lastEdited", "uploaded", "_key")

    ## individuals
    if(!is.null(individuals.input)){
        toRemove <- unlist(lapply(hidden.fields, function(e) grep(e, names(individuals.input),ignore.case=TRUE)))
        individuals.input <- individuals.input[,-toRemove,drop=FALSE]
    }

    ## records
    if(!is.null(records.input)){
        for(i in 1:length(records.input)){
            toRemove <- unlist(lapply(hidden.fields, function(e) grep(e, names(records.input[[i]]),ignore.case=TRUE)))
            records.input[[i]] <- records.input[[i]][,-toRemove,drop=FALSE]
        }
    }


    ## BUILD OBJECT AND RETURN ##
    out <- new("obkData", individuals=individuals.input, records=records.input,
               contacts=fromto, contacts.start=date.start, contacts.end=date.end)
    return(out)
} # end JSON2obkData
