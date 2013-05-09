## wrapper for plot functions
##
##author: Rolf Ypma, amended by Thibaut Jombart

setMethod("plot", "obkData", function(x, y=c("timeline","geo","mst","phylo"), ...){
    y <- match.arg(y)
    if(y=="timeline")
        plotIndividualTimeline(x,...)
    else if(y=="geo")
        plotGeo(x,...)
    else if(y=="mst")
        plotggMST(x,...)
    else if(y=="phylo")
        plotggphy(x,...)
    else
        warning("Type (argument 'y') is not recognized. Valid types: timeline, geo, mst, phylo")
})




