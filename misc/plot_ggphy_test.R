test<-function(){

dir<-"."
file<-"H3_WD_n1000_GTRGI_E_skyline.trees_time_MCC.txt"
#create a phylo
	my_phylo<-read.nexus(file=paste(dir,file,sep="/"))
	phylo<-ladderize(my_phylo)
	tip_dates<-as.Date(extract_string(phylo$tip.label,"_",2))
	my_ggphy<-phylo2ggphy(phylo,tip_dates=tip_dates,branch_unit="year")
	
	tip_attributes<-data.frame(tip_id=phylo$tip.label,tip_colour=as.factor(sample(1:5,size=length(tip_dates),replace=T)))
	
	p<-plot_ggphy(my_ggphy,tip_labels=F,tip_attribute=tip_attributes,var_tip_labels="tip_id",var_tip_colour="tip_colour")
	
	p<-plot_ggphy(my_ggphy,tip_labels=F,tip_attribute=NULL,var_tip_labels=NULL,var_tip_colour=NULL)
	
	
}
