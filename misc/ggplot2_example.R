library(ggplot2)
library(plyr)
library(reshape2)

#setwd("/Users/Oliver/git/git_outbreak/misc")
#source(ggplot2_example.R)


outbreakg.aes_now<- function(...) 
{
	structure(list(...),  class = "uneval")
}


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
	x<- ts.sim()
	print(ts.plot(x,x="time",y="measure",col="sex",facet="age~school"))
}