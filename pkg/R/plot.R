#wrapper for plot functions
#
#author: Rolf Ypma

plot.obkData=function(x,type='timeline',...){
	if(class(x)!='obkData'){
		error('this function only plots obkData objects')
	}
	if(type=='timeline')
		plotIndividualTimeline(x,...)
	else if(type=='geo')
		plotGeo(x,...)
	else if(type=='mst')
		plotggMST(x,...)
	else if(type=='phylo')
		plotggphy(x,...)
	else
		warning('type was not recognized. Valid types: timeline, geo, mst, phylo')
}




