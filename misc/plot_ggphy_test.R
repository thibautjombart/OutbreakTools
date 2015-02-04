source("/Users/tonton/Documents/Project/Rfiles/Utils.R")
library(ggplot2)
library(scales)
setwd("/Users/tonton/Documents/GitProjects/OutbreakTools/code/misc")
source("/Users/tonton/Documents/GitProjects/OutbreakTools/code/pkg/R/phylo2ggphy.R")
source("/Users/tonton/Documents/GitProjects/OutbreakTools/code/pkg/R/plotggphy.R")

library(OutbreakTools)

test<-function(){

    dir<-"."
    file<-"H3_WD_n1000_GTRGI_E_skyline.trees_time_MCC.txt"
                                        #create a phylo
    my_phylo<-read.nexus(file=paste(dir,file,sep="/"))
    phylo<-ladderize(my_phylo)
    
    p<-OutbreakTools:::plotggphy(phylo,branch.unit="year",guess.tip.dates.from.labels=T,set.guess=list(prefix="_",order=2))
    
    tip_dates<-as.Date(extract_string(phylo$tip.label,"_",2))
    tip_attribute<-data.frame(label=phylo$tip.labe,tip.dates=tip_dates,tip.col=1:length(tip_dates))
    p<-OutbreakTools:::plotggphy(phylo,branch.unit="year",tip.attribute=tip_attribute,tip.colour="tip.col",tip.dates="tip.dates")
    
    #H1N1 example
    phylo<-read.nexus("../../pandemic_geo.mcc")
    location<-.extract.string(phylo$tip.label,"_",2,from="last")        
    tip_attribute<-data.frame(label=phylo$tip.labe,location=location,dummy_shape=sample(LETTERS[1:3],length(location),replace=T))
    p<-plotggphy(phylo,ladderize=T,branch.unit="year",tip.attribute=tip_attribute,tip.colour="location",tip.size=3,tip.shape="dummy_shape",tip.alpha=0.75,guess.tip.dates.from.labels=T,set.guess=list(prefix="_",order=1,from="last"),axis.date.format="%b%Y",major.breaks=NULL,minor.breaks="months",colour.palette="Spectral",legend.position="right")

    
    load("../pkg/data/singapore.Rdata")	
    my_ggphy2<-phylo2ggphy(trees)
    p<-plot_ggphy(my_ggphy2,tip_labels=T)
    
    
}
