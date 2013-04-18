#' Function to plot a timeline for individuals involved in an outbreak in one plot
#'
#' @param x the main obkData object
#' @param selection a vector of integers indicating the subset of individuals to plot
#' @param ordering a vector of the same length as selection, which specifies the order of individuals on the plot. Overridden by orderBy
#' @param orderBy string giving the name of the column that should be used to order the individuals. Overrides ordering
#' @param colorBy string giving the name of the column by which individuals should be coloured
#' @param events vector of strings giving the names of the columns that hold date values in 'individuals', to be marked on the time line
#' @param clinicalEvents vector of strings giving the names of the columns that hold date values in 'clinical', to be marked on the time line
#' @param periods an Nx2 matrix of strings, giving pairs of column names for periods to be plotted
#' @param plotSamples should the samples (at sampling time) be plotted?
#' @param plotNames should the individualIDs be shown at the y-axis?
#' @export
#' @author Rolf Ypma
#'
#' @examples #load equine influenza data and convert it to obkData
#' data(HorseFlu)
#' data <- new("obkData", individuals=HorseFlu$individuals,samples=HorseFlu$samples,clinical=HorseFlu$clinics)
#' #plot the horses over time, coloring by yard
#' plot.individualTimeline(data,colorBy='yardID')
#' #also sort on yard
#' plot.individualTimeline(data,orderBy='yardID',colorBy='yardID')
#' #just plot the first 15
#' plot.individualTimeline(data,selection=1:15,orderBy='yardID',colorBy='yardID')
#' #do a simple plot for some influenza data
#' data(fakefludata)
#' data <- new("obkData", individuals=Patientsdata,samples=samplefludata,clinical=clinicalfludata)
#' plot.individualTimeline(data,colorBy='gender')

meltDateProof <- function(data,id.vars,measure.vars,variable.name){
	# 'melt' in the reshape package looses the Date format, here is a hack around
	if(class(data[,measure.vars[1]])=="Date"){
		for(e in measure.vars){
			#convert all columns (which should be dates) to chars
			data[,e] <- as.character(data[,e])
		}
		df <- melt(data,id.vars=id.vars,measure.vars=measure.vars,variable.name=variable.name)
		#and convert back to Date
		df$value <- as.Date(df$value)
	}
	else
		df <- melt(data,id.vars=id.vars,measure.vars=measure.vars,variable.name=variable.name)
	return(df)
}


plotIndividualTimeline <- function(x, selection=1:dim(get.data(x,'individuals'))[1],
                                    ordering=1:length(selection), orderBy=NULL, colorBy=NULL,
                                    events=NULL, clinicalEvents=NULL,periods=NULL,
                                    plotSamples=TRUE, plotNames=length(selection)<50){
	## plot selection of the individuals in data as a line, ordered by ordering
	## color by colorby
	## make lines for periods, an Nx2 matrix of column names
	## clinical Events is a vector specifying which of the dataframes in the clinicalData list to plot

	if(length(selection)>length(get.individuals(x))){
		warning("selection is longer than the number of individuals. Selecting all.")
		selection=1:length(get.individuals(x))
	}

	if(!is.null(orderBy)){
		## alternatively, order by this character
		ordering <- sapply(1:length(selection), function(i) which(order(get.data(x,orderBy)[selection])==i))
	}

	df <- x@individuals[selection,,drop=FALSE]#TODO subset() should do this, but gave an error (21-2-2013)
		## add the ordering with an outrageous name so we don't override
	df$yITL <- ordering

	## the plot itself
	if(plotNames)
		plotIndTL <- ggplot(data=df)+scale_y_discrete(name="Individuals",breaks=1:length(ordering),labels=get.individuals(x)[selection][ordering])
	else
		plotIndTL <- ggplot(data=df)+scale_y_discrete(name="Individuals",breaks=NULL)

	## with coloring if wanted
	if(is.null(colorBy)){
		## first, plot a weak background line
		plotIndTL <- plotIndTL+geom_hline(aes(yintercept=yITL),alpha=.3)
	}
	else{
		## first, plot a weak background line
		plotIndTL <- plotIndTL+geom_hline(aes_string(yintercept="yITL",colour=colorBy),alpha=I(.3))
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

	## plot events
	if(plotSamples){
		## make the data frame with sample dates
		dfSample=get.data(x,'samples')
		if(!is.null(dfSample)){
			yVals=rep(0,length(dfSample[,1]))
			for(i in 1:length(dfSample[,1])){#assign the y values
				yVals[i]=ordering[which(get.individuals(x)==dfSample$individualID[i])]#TODO this matching should be a standard method
			}
			dfSample$yITL=yVals
			## make a new df using melt as an input for ggplot, so we can show the different types of events
			fulldf <- meltDateProof(dfSample,id.vars='yITL',measure.vars='date',variable.name="type")
		}
	}
	else{
		fulldf=c()
	}
	if(!is.null(events)){
		## TODO how to get attributes from different dataframes when columns have the same name? e.g. 'date' from samples and clinical
		## add more events from 'individuals' to be drawn to the dataframe fulldf
		fulldf <- rbind(fulldf,meltDateProof(df,id.vars='yITL',measure.vars=events,variable.name="type"))
	}
	if(!is.null(clinicalEvents)){
		## add clinical events to be drawn to the dataframe fulldf
## 		clinicalDF <- data@clinical[selection,,drop=FALSE]
## 		clinicalDF$yITL <- ordering
## 		fulldf <- rbind(fulldf,meltDateProof(clinicalDF,id.vars='yITL',measure.vars=clinicalEvents,variable.name="type"))
	## TODO 21-2-2013: for this to work we need an accessor for clinicals, that can hande duplicate column names and can subset
	}
	if(!is.null(fulldf)){
		## draw the events
		plotIndTL <- plotIndTL+geom_point(data=fulldf,aes(y=yITL,x=value,colour=c(),shape=type))
	}
	plotIndTL
}


