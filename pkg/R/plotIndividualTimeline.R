#' Function to plot a timeline for individuals involved in an outbreak in one plot
#'
#' @param data the main dataframe holding data on the individuals
#' @param selection a vector of integers indicating the subset of the dataframe to use.
#' @param ordering a vector of the same length as selection, which specifies the order of individuals on the plot. Overridden by orderBy
#' @param orderBy string giving the name of the column that should be used to order the individuals. Overrides ordering
#' @param colorBy string giving the name of the column by which individuals should be coloured
#' @param events vector of strings giving the names of the columns that hold data values, to be marked on the time line
#' @param clinicalEvents vector of integers giving the indices of the list clinicalData that should be plotted
#' @param periods n X 2 matrix of strings giving the pairs of names for Date columns that should be plotted as periods on the time line
#' @export
#' @author Rolf Ypma
#' @examples plotIndividualTimeline(individualData,orderBy="exposure.code",selection=1:20,clinicalEvents = 1:4,colorBy="exposure.code")


#library(ggplot2)
#library(grid)
#library(ape)
#library(reshape2)

meltDateProof <- function(data,id.vars,measure.vars,variable.name){
    ## melt looses the Date format, here is a hack around
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




plotIndividualTimeline <- function(data, selection=1:dim(data)[1], ordering=1:length(selection),
                                   orderBy=NULL, colorBy=NULL, events=NULL, clinicalEvents=NULL,
                                   periods=NULL, drawSamples=FALSE){
    ##plot selection of the individuals in data as a line, ordered by ordering
    ##color by colorby
    ##make lines for periods, a Nx2 matrix
    ##clinical Events is a vector specifying which of the dataframes in the clinicalData list to plot

    data=data[selection,]

    if(!is.null(orderBy)){
        ##alternatively, order by this character
        ordering <- sapply(1:dim(data)[1],function(x) which(order(data[,orderBy])==x))
    }

    ##add the y variable with an outrageous name so we don't override
    data$yITL <- ordering

    ##the plot itself
    plotIndTL <- ggplot(data=data)+scale_y_discrete(name="Individuals",breaks=NULL)
    ##with coloring if wanted
    if(is.null(colorBy)){
        ##first, plot a weak background line
        plotIndTL <- plotIndTL+geom_hline(aes(yintercept=yITL,alpha=.3))
    }
    else{
        ##first, plot a weak background line
        plotIndTL <- plotIndTL+geom_hline(aes_string(yintercept="yITL",alpha=I(.3),colour=colorBy))
    }

    if(!is.null(periods)){
        if(class(periods)!="matrix"){
            warning("periods should be nx2 matrix of colnames")
        }
        else{
            for(i in 1:dim(periods)[1]){
                if(is.null(colorBy))
                    plotIndTL <- plotIndTL+geom_segment(aes_string(y="yITL",yend="yITL",x=periods[i,1],xend=periods[i,2]),size=1,lineend="round",alpha=.5)
                else
                    plotIndTL <- plotIndTL+geom_segment2(aes_string(y="yITL",yend="yITL",x=periods[i,1],xend=periods[i,2],colour=colorBy),size=I(1),alpha=.5,lineend="round")

            }
        }
    }


    fulldf <- data #a larger dataframe which events can be added to
                                        #	if(!is.null(clinicalEvents)){
                                        #plot the clinical events, shaped by type
    colsToDo=c()#in this, save the column names with dates we need later
    for (i in clinicalEvents) {
        clindf <- clinicalData[[clinicalEvents[i]]][selection,]
        ## merge the dataframs of individuals and clinical events
        if(dim(clindf)[2]==2){
            ## assume it's only one date, plot it with a shape#TODO assumption correct?
            fulldf=merge(fulldf,clindf,by="individualID")
            colsToDo <- c(colsToDo,names(clindf)[2])
        }
        else if(dim(clindf)[2]==3){
            ## assume it's a period, plot that in the individual's color #TODO assumption correct?
            ## TODO color per period type?
            ## TODO does not work if the merge renames columns
            if(is.null(colorBy))
                plotIndTL <- plotIndTL+geom_segment(data=merge(data,clindf,by="individualID"),aes_string(y="yITL",yend="yITL",x=names(clindf)[2],xend=names(clindf)[3]),size=1,lineend="round",alpha=.5)
            else
                plotIndTL <- plotIndTL+geom_segment2(data=merge(data,clindf,by="individualID"),aes_string(y="yITL",yend="yITL",x=names(clindf)[2],xend=names(clindf)[3],colour=colorBy),size=I(1),alpha=.5,lineend="round")

        }
    }

                                        #	}
    if(drawSamples){
        temps <- samples
        names(temps)[which(names(samples)=="date")] <- "sampleDate"#rename
        fulldf=merge(fulldf,temps,by="individualID")
        colsToDo <- c(colsToDo,"sampleDate")
    }
    ## plot all events
    if(!is.null(c(colsToDo,events))){
        ## make a new df using melt as an input for ggplot
        fulldf <- meltDateProof(fulldf,id.vars='yITL',measure.vars=c(colsToDo,events),variable.name="type")
        plotIndTL <- plotIndTL+geom_point(data=fulldf,aes(y=yITL,x=value,colour=c(),shape=type))
    }

    print(plotIndTL)
    plotIndTL
}

