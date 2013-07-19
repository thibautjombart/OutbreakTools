


## hack to remove the NOTE in R CMD check about:
## plotIndividualTimeline: no visible binding for global variable ‘yITL’
## plotIndividualTimeline: no visible binding for global variable ‘value’
## plotIndividualTimeline: no visible binding for global variable ‘type’
if(getRversion() >= "2.15.1")  utils::globalVariables(c("yITL","value","type"))



## Function to plot a timeline for individuals involved in an outbreak in one plot

.meltDateProof <- function(data,id.vars,measure.vars,variable.name){
    ## 'melt' in the reshape package looses the Date format, here is a hack around
    if(class(data[,measure.vars[1]])=="Date"){
        for(e in measure.vars){
            ## convert all columns (which should be dates) to chars
            data[,e] <- as.character(data[,e])
        }
        df <- melt(data,id.vars=id.vars,measure.vars=measure.vars,variable.name=variable.name)
        ## and convert back to Date
        df$value <- as.Date(df$value)
    }
    else
        df <- melt(data,id.vars=id.vars,measure.vars=measure.vars,variable.name=variable.name)
    return(df)
}


##########################
## plotIndividualTimeline
##########################
plotIndividualTimeline <- function(x, what, selection=NULL,
                                   ordering=1:length(selection), orderBy=NULL, colorBy=NULL,
                                   events=NULL, periods=NULL, plotNames=length(selection)<50){
    ## plot selection of the individuals in data as a line, ordered by ordering
    ## color by colorby
    ## make lines for periods, an Nx2 matrix of column names
    ## clinical Events is a vector specifying which of the dataframes in the clinicalData list to plot

    if(is.null(selection)) selection <- 1:get.nindividuals(x, "individuals")
    if(length(selection)>length(get.individuals(x))){
        warning("selection is longer than the number of individuals. Selecting all.")
        selection <- 1:get.nindividuals(x, "individuals")
    }

    ## GET THE DATA.FRAME FOR THE TIME SERIES ##
    ## get subset of the plotted data ##
    x <- subset(x, individuals=selection)

    ## define ordering ##
    if(!is.null(orderBy)){
        ## alternatively, order by this character
        ordering <- sapply(1:length(selection), function(i) which(order(get.data(x,orderBy)[selection])==i))
    }

    ## get time series data.frame ##
    df.ts <- make.individual.attributes(x)[ordering, , drop=FALSE]
    individualID <- df.ts$individualID

    ## isolates dates matching with requested data
    toKeep <- unlist(lapply(what, grep, names(df.ts)))
    if(length(toKeep)<1) stop(paste(what, "was not found in the data"))
    df.ts <- df.ts[, toKeep,drop=FALSE]

    ## keep only dates
    toKeep <- sapply(df.ts, inherits, "Date")
    if(length(toKeep)<1) stop("No date information to be used for the timeline plot")
    df.ts <- df.ts[, toKeep,drop=FALSE]

    ## melt into long form, careful to convert dates to characters and back
    isDate <- sapply(df.ts, inherits, "Date")
    for(i in 1:ncol(df.ts)) if(inherits(df.ts[[i]], "Date")) df.ts[[i]] <- as.character(df.ts[[i]])

    ## add individualID and melt
    df.ts$individualID <- individualID
    df.ts$yITL <- 1:nrow(df.ts)
    df.ts <- melt(df.ts, id.var=c("individualID","yITL"))

    ## remove NAs, restore Date class
    df.ts <- na.omit(df.ts)
    df.ts$value <- .process.Date(df.ts$value)
    names(df.ts) <- c("individualID", "yITL", "type", "date")


    ## GET THE DATA.FRAME FOR THE INDIVIDUAL LINES ##
    df.ind <- x@individuals[ordering,,drop=FALSE]
    df.ind$yITL <- 1:nrow(df.ind)

    ## keep only relevant individuals
    df.ind <- df.ind[unique(df.ts$individualID),,drop=FALSE]
    df.ind$individualID <- rownames(df.ind)


    ##df.full <- merge(df.ts, df.ind, by="individualID", all.x=TRUE, all.y=FALSE, sort=FALSE) # not needed


    ## BUILD THE PLOT ##
    ## base = time line
    out <- ggplot(df.ind)

    ## add horizontal lines for individuals
    if(is.null(colorBy)){
        out <- out + geom_hline(aes(yintercept=yITL),alpha=.3)
    } else{
        out <- out + geom_hline(aes_string(yintercept="yITL", colour=colorBy, data=df.ind),alpha=I(.3), data=df.ind)
    }

    ## add points
    out <- out + geom_point(aes(x=date, y=yITL, shape=type), data=df.ts)


    ## add indiv labels
    if(plotNames)
        out <- out + scale_y_discrete(aes(name="Individuals", label=IndividualID))
    else
        out <- out + scale_y_discrete(name="Individuals",breaks=NULL)


    ## THIS MAY NEED TESTING
    if(!is.null(periods)){
        if(class(periods)!="matrix"){
            warning("periods should be nx2 matrix of colnames")
        }
        else{
            for(i in 1:dim(periods)[1]){
                if(is.null(colorBy))
                    out <- out+geom_segment(aes_string(y="individualID",yend="individualID",x=periods[i,1],xend=periods[i,2]),size=1,lineend="round",alpha=.5)
                else
                    out <- out+geom_segment(aes_string(y="individualID",yend="individualID",x=periods[i,1],xend=periods[i,2],colour=colorBy),size=I(1),alpha=.5,lineend="round")

            }
        }
    }



    ## plot events
        ## make the data frame with sample dates ##
        dfRecords <- make.individual.attributes(x)

        ## keep only dates
        toKeep <- sapply(dfRecords, inherits, "Date")
        if(length(toKeep)<1) stop("No date information to be used for the timeline plot")
        dfRecords <- dfRecords[,toKeep,drop=FALSE]

        ## convert dates to characters for now
        dfRecords <- as.data.frame(lapply(dfRecords, as.character))

        ## keep only matches with requested data
        toKeep <- unlist(lapply(what, grep, names(dfRecords)))
        if(length(toKeep)<1) stop(paste(what, "was not found in the data"))
        dfRecords <- dfRecords[,toKeep,drop=FALSE]

        ## add yITL info
        dfRecords$yITL <- ordering

        ## convert data.frame into 'long' form
        dfRecords <- melt(dfRecords, id.var="yITL")

        ## remove NAs, restore Date class
        dfRecords <- na.omit(dfRecords)
        dfRecords$value <- .process.Date(dfRecords$value)

        ##select the records corresponding to our selection of individuals
        dfRecords=dfRecords[dfRecords$individualID%in%get.individuals(x),]
        ##one sample can have several rows in this df, take only unique ones
        dfRecords=dfRecords[sapply(unique(dfRecords$sampleID),function(y){min(which(dfRecords$sampleID==y))}),]
        if(!is.null(dfRecords)){
            dfRecords$yITL=ordering[sapply(dfRecords$individualID,function(y){which(get.individuals(x)==y)})]#TODO this matching should be a standard method

            ## make a new df using melt as an input for ggplot, so we can show the different types of events
            fulldf <- .meltDateProof(dfRecords,id.vars='yITL',measure.vars='date',variable.name="type")
            fulldf$type=rep('sample',dim(fulldf)[1])#this could be done a lot easier...
        }

    else{
        fulldf=c()
    }
    if(!is.null(events)){
        ## TODO how to get attributes from different dataframes when columns have the same name? e.g. 'date' from records and clinical
        ## add more events from 'individuals' to be drawn to the dataframe fulldf
        fulldf <- rbind(fulldf,.meltDateProof(df,id.vars='yITL',measure.vars=events,variable.name="type"))
    }
    ## if(!is.null(clinicalEvents)){
    ## 	## add clinical events to be drawn to the dataframe fulldf
    ## 	## 		this will have to be done with the new functionality of get.data. See ticket 35 (11-4-2013)
    ## 	#clinicalDF <- data@clinical[selection,,drop=FALSE]
    ## 	## 		clinicalDF$yITL <- ordering
    ## 	## 		fulldf <- rbind(fulldf,.meltDateProof(clinicalDF,id.vars='yITL',measure.vars=clinicalEvents,variable.name="type"))
    ## 	## TODO 21-2-2013: for this to work we need an accessor for clinicals, that can hande duplicate column names and can subset
    ## }

    if(!is.null(fulldf)){
        ## draw the events
        out <- out+geom_point(data=fulldf,aes(y=yITL,x=value,colour=c(),shape=type))
    }
    suppressWarnings(out)
}


