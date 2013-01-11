library(ggplot2)
library(plyr)
library(reshape2)

#setwd("/Users/Oliver/git/git_outbreak/misc")
#source(ggplot2_example.R)


#' create mock time series
#'
#' @return default mock time series
#' @example x<- ts.sim();
ts.sim<- function()
{
	time	<-as.Date("2013-01-01")+1:100
	tmp		<-expand.grid(age=c("0-5","6-10","11-15",">15"),school=c("Sch A","Sch B","Sch C"),sex=c("F","M"))	
	df_mock	<-ddply(tmp,c("age","school","sex"),function(df)
			{	
				return(data.frame(time,measure=rnorm(length(time))))		
			})
	df_mock
}

#' convert data frame of individuals to data frame of population
#' @example load("/Users/Oliver/git/git_outbreak/pkg/data/fake_SARS_HK.Rdata")
#' @example df<- ts.create.from.individual(fake_SARS_HK, individual="Patient.ID", time.st="Admission", time.e=c("Discharge","Death"), other="level")
#' @example print(ts.plot(df,x="time",y="I",col="level"))
ts.create.from.individual<- function(df, individual=NULL, time.st=NULL, time.e=NULL, other=NULL, time.format="d/m/y")
{
	if(is.null(individual))				individual<- colnames(df)[1]
	if(is.null(time.st))				time.st<- colnames(df)[2]
	if(is.null(time.e))					time.e<- colnames(df)[3]
	if(!is.character(individual))		stop("individual not a character")
	if(!is.character(time.st))			stop("time.st not a character")
	if(!is.character(time.e))			stop("time.e not a character")
	
	#collapse to complete information
	time.e.cols		<- lapply(time.e, function(x)		as.Date(ts.ISOdate_format(as.character(df[,x]),type=time.format))	)
	tmp<- !sapply(time.e.cols,is.na)
	
	tmp<- lapply( seq_len(ncol(tmp)),function(i)	df[tmp[,i],] )
	print(tmp)
	stop()
	sapply(seq_len())
	
	tmp				<- which(apply(!sapply(time.e.cols,is.na),1,any))
	df<- df[tmp,]
	
	time.e.cols		<- sapply(time.e.cols, function(x) x[tmp])
	tmp<- apply(time.e.cols,1,function(x) which.min(x))		
	
	print( df[,time.e.cols][,tmp] )
	stop()
	time.st.col		<- ts.ISOdate_format(as.character(df[,time.st]),type=time.format)
	time.st.col		<- as.Date( time.st.col )
	#time.st.origin	<- min(time.st.col)
	xlim<- range(time.st.col, na.rm=1)
	print(xlim)
	stop()
	
	time.e.cols		<- sapply(function(x) x[tmp])
	print(tmp)
	
	#time.e.origin	<- sapply(time.e.cols,function(x) range(x, na.rm=1)	)
	
	
	#print(time.e.origin)		
	stop()
	#print( range(df[,c(time.st,time.e)]) )
}

#' create time series plot 
#'
#' @param df data frame with all information to be plotted
#' @param x column name of column that should be on 'x' axis. If NULL, the first column is taken.
#' @param y column name of column that should be on 'y' axis. If NULL, the second column is taken.
#' @param facet column name or formula to create subplots. See examples. If NULL, no subplot is created.
#' @param col column name to create sub time series in the same plot. See examples. If NULL, no color is used.
#' @return ggplot2 object
#' @example x<- ts.sim(); print(ts.plot(x,x="time",y="measure",col="sex",facet="age~school"))
ts.plot<- function(df, x=NULL, y=NULL, facet=NULL,col=NULL)
{
	if(is.null(x))	x<- colnames(df)[1]
	if(is.null(y))	y<- colnames(df)[2]
	if(!is.character(x))	stop("x not a character")
	if(!is.character(y))	stop("y not a character")
	p<- ggplot(data=df,aes_string(x=x,y=y))
	if(!is.null(facet))
	{
		if(!regexpr("~",facet,fixed=1)>0)
			facet<- paste(facet,"~.",sep='')		
		p<- p+facet_grid(facet, scales="free")
	}
	if(!is.null(col))
		p<- p+geom_line(aes_string(colour=col))
	else
		p<- p+geom_line()	
	p
}

#main
if(1)
{
	#x<- ts.sim()
	#print(ts.plot(x,x="time",y="measure",col="sex",facet="age~school"))
	source("../pkg/R/ISOdate_format.R")
	load("/Users/Oliver/git/git_outbreak/pkg/data/fake_SARS_HK.Rdata")
	ts.create.from.individual(fake_SARS_HK, individual="Patient.ID", time.st="Admission", time.e=c("Discharge","Death"))
}