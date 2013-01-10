library(ggplot2)
library(plyr)
library(reshape2)

#setwd("/Users/Oliver/git/git_outbreak/misc")
#source(ggplot2_example.R)


outbreakg.aes_now<- function(...) 
{
	structure(list(...),  class = "uneval")
}

#create mock data set
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

ts.plot<- function(df, x=NULL, y=NULL, facet=NULL,col=NULL)
{
	if(is.null(x))	x<- colnames(df)[1]
	if(is.null(y))	y<- colnames(df)[2]
	if(!is.character(x))	stop("x not a character")
	if(!is.character(y))	stop("y not a character")
	p<- ggplot(data=df,aes_string(x=x,y=y))
	if(!is.null(facet))
		p<- p+facet_grid(age~school)
	
	if(!is.null(col))
		p<- p+geom_line(aes_string(colour=col))
	else
		p<- p+geom_line()	
	#p<-p+facet_grid(age~school)
	#p<-p+geom_line(aes(colour=sex))
	#print(p)
	p
}

#main
if(1)
{
	df<- ts.sim()
	print(ts.plot(df,x="time",y="measure",col="sex"))
}