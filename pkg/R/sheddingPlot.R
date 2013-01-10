#example of plotting a continuous variable (e.g., viral load) that has been measured over time
# and for which we have structured data (in different wards, yards, farms ....)

shedding.plot<- function(df, x=NULL, y=NULL, facet=NULL,col=NULL)
{
  if(is.null(x))	x<- colnames(df)[1]
  if(is.null(y))	y<- colnames(df)[2]
  if(is.null(col))  col<- colnames(df)[3]
  if(!is.character(x))	stop("x not a character")
  if(!is.character(y))	stop("y not a character")  
  if(!is.character(col))  stop("col not a character")
  #might be able to remove this depending on how we deal with times
  df[,x]<-as.Date(df[,x],format="%d/%m/%Y")
  p<- ggplot(data=df,aes_string(x=x,y=y,fill=col))
  if(!is.null(facet))
  {
    
    if(!regexpr("~",facet,fixed=1)>0)
      facet<- paste(facet,"~.",sep='')		
    p<- p+facet_grid(facet)
  }
  if(!is.null(col))
    p<- p+geom_bar(aes_string(colour=col),stat="identity")
  else
    p<- p+geom_bar(stat="identity")
  p
}

#main
#if(1)
#{
#  print(shedding.plot(HorseFluShedding,x="SwabDate",y="CopyNo",col="YardID",facet="YardID"))
#}