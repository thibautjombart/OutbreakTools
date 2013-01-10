library(ggplot2)
library(plyr)
library(reshape2)

#create mock data set
time<-as.Date("2013-01-01")+1:100
tmp<-expand.grid(age=c("0-5","6-10","11-15",">15"),school=c("Sch A","Sch B","Sch C"),sex=c("F","M"))


df_mock<-ddply(tmp,c("age","school","sex"),function(df){
	
return(data.frame(time,measure=rnorm(length(time))))	
	
})



p<-ggplot(data=df_mock,aes(x=time,y=measure))
p<-p+facet_grid(age~school)
p<-p+geom_line(aes(colour=sex))
print(p)