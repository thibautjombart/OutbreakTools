


## hack to remove the NOTE in R CMD check about:
## plotIndividualTimeline: no visible binding for global variable ‘yITL’
## plotIndividualTimeline: no visible binding for global variable ‘value’
## plotIndividualTimeline: no visible binding for global variable ‘type’
if(getRversion() >= "2.15.1")  utils::globalVariables(c("yITL","value","type"))



## Function to plot a timeline for individuals involved in an outbreak in one plot

.meltDateProof <- function(data,id.vars,measure.vars,variable.name){
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


plotIndividualTimeline <- function(x, selection=1:length(get.individuals(x)),
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
	#only take this subset of the data
	x=subset(x,selection)
	if(!is.null(orderBy)){
		## alternatively, order by this character
		ordering <- sapply(1:length(selection), function(i) which(order(get.data(x,orderBy)[selection])==i))
	}

	df <- x@individuals[,,drop=FALSE]
	## add the ordering with an outrageous name so we don't override
	df$yITL <- ordering

	## the plot itself
	if(plotNames)
		plotIndTL <- ggplot(data=df)+scale_y_discrete(name="Individuals",breaks=1:length(ordering),labels=get.individuals(x)[ordering])
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
					plotIndTL <- plotIndTL+geom_segment(aes_string(y="yITL",yend="yITL",x=periods[i,1],xend=periods[i,2],colour=colorBy),size=I(1),alpha=.5,lineend="round")

			}
		}
	}



	## plot events
	if(plotSamples){
		## make the data frame with sample dates
		dfSamples=get.data(x,'samples')
		#select the samples corresponding to our selection of individuals
		dfSamples=dfSamples[dfSamples$individualID%in%get.individuals(x),]
		#one sample can have several rows in this df, take only unique ones
		dfSamples=dfSamples[sapply(unique(dfSamples$sampleID),function(y){min(which(dfSamples$sampleID==y))}),]
		if(!is.null(dfSamples)){
			dfSamples$yITL=ordering[sapply(dfSamples$individualID,function(y){which(get.individuals(x)==y)})]#TODO this matching should be a standard method

			## make a new df using melt as an input for ggplot, so we can show the different types of events
			fulldf <- .meltDateProof(dfSamples,id.vars='yITL',measure.vars='date',variable.name="type")
			fulldf$type=rep('sample',dim(fulldf)[1])#this could be done a lot easier...
		}
	}
	else{
		fulldf=c()
	}
	if(!is.null(events)){
		## TODO how to get attributes from different dataframes when columns have the same name? e.g. 'date' from samples and clinical
		## add more events from 'individuals' to be drawn to the dataframe fulldf
		fulldf <- rbind(fulldf,.meltDateProof(df,id.vars='yITL',measure.vars=events,variable.name="type"))
	}
	if(!is.null(clinicalEvents)){
		## add clinical events to be drawn to the dataframe fulldf
		## 		this will have to be done with the new functionality of get.data. See ticket 35 (11-4-2013)
		#clinicalDF <- data@clinical[selection,,drop=FALSE]
		## 		clinicalDF$yITL <- ordering
		## 		fulldf <- rbind(fulldf,.meltDateProof(clinicalDF,id.vars='yITL',measure.vars=clinicalEvents,variable.name="type"))
		## TODO 21-2-2013: for this to work we need an accessor for clinicals, that can hande duplicate column names and can subset
	}

	if(!is.null(fulldf)){
		## draw the events
		plotIndTL <- plotIndTL+geom_point(data=fulldf,aes(y=yITL,x=value,colour=c(),shape=type))
	}
	suppressWarnings(plotIndTL)
}


