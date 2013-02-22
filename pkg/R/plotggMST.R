#' Function to plot a minimum spanning tree using ggplot from a the class 'obkData'
#'
#' @param x an object of the class "obkData"
#' @export
#' @author Joseph Hughes
#' @examples see below

plotggMST<-function(x,individualID=NULL,locus=NULL){
  if(get.nlocus(x)==0){
    warning("No DNA sequences in the data.")
    return(NULL)
  }
  ## GET DNA SEQUENCES ##
  if(!is.null(locus) && get.nlocus(x)>1){
    warning("You need to provide the locus name")
    return(NULL)
  }
  if(is.null(locus) && get.nlocus(x)==1) locus <- 1
  print (locus)
  
  if (!is.null(locus)){
    subx<-subset(x, locus=locus)
    subx<-subset(subx, individuals=individualID)
    subdna<-get.dna(subx)[[1]]
    seqmatrix <- as.character(subdna)
    seqstring<-apply(format(seqmatrix), 1, paste, collapse="")
    uniqList<-tapply(names(seqstring),list(seqstring),I)
    splitseqstr<-strsplit(names(uniqList),"")
    uniqmat<-do.call(rbind, splitseqstr)
    uniqnames<-paste("uniqseqID",seq(1:length(uniqList)),sep="")
    rownames(uniqmat)<-uniqnames
    names(uniqList)<-uniqnames
    uniqdna<-as.DNAbin(uniqmat)
    # get the counts for each sequence
    IDcounts<-do.call(rbind, lapply(uniqList, function(x) length(x)))
    IDcounts<-as.data.frame(IDcounts[order(-IDcounts[,1]),])
    colnames(IDcounts) <- c( 'count') 
    seqindex<-match(rownames(IDcounts), labels(uniqdna))
    # reorder the DNAbin accordingly
    ordereddna<-uniqdna[seqindex, ]
    # print(ordereddna)
    uniqdist<-dist.dna(ordereddna,model="raw", as.matrix=TRUE)
    mstdist<-mst(uniqdist)
    plotcord <- data.frame(gplot.layout.fruchtermanreingold(mstdist, NULL))
    X1=X2=Y1=Y2=NULL
    colnames(plotcord) = c("X1","X2")
    rownames(plotcord) = rownames(uniqdist)
    # print((plotcord))
    # print((IDcounts))
    plotcord<-cbind(plotcord,IDcounts)

    # print((plotcord))
    mstdist[lower.tri(mstdist,diag=TRUE)]<-NA
    eList <- NULL
    for ( i in 1:nrow(mstdist) ){
      for ( j in 1:ncol(mstdist)) {
        if (!is.na(mstdist[i,j])){
          if (mstdist[i,j]>0){
          # print(uniqdist[i,j])
          # print(mstdist[i,j])
          eList <- rbind(eList,c( rownames(mstdist)[i], colnames(mstdist)[j]))
          }
        }
      }
    }
    #eList
    # print(eList)
    # edges of zero are removed
    #emst<-subset(eList,eList[,3]>0)
    edges <- data.frame(plotcord[eList[,1],1:2], plotcord[eList[,2],1:2])
    
    colnames(edges) <-  c("X1","Y1","X2","Y2")
    # print(edges)
    
    old <- theme_set(theme_bw())
    old<-theme_update(
               axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),
               axis.title.y = element_blank(),	panel.grid.major.y = element_blank(),
               panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank(), panel.border = element_blank(), 
               panel.background = element_blank(), legend.position = "none")

    pmst<-ggplot()
    pmst<-pmst+geom_segment(data=edges,aes(x=X1,xend=X2,y=Y1,yend=Y2),lineend="round")
    pmst<-pmst+scale_y_continuous("",labels=NULL)+scale_x_continuous("",labels=NULL)
    pmst<-pmst+geom_point(aes(X1, X2, size=count, colour="red"), data=plotcord)
  }
  #print(pmst)
  return(pmst)
}

##################
####  TESTING ####
##################
## NOTE: THIS MUST BE COMMENTED WHEN COMPILING/INSTALLING THE PACKAGE


# data(HorseFlu)
# attach(HorseFlu)
# x <- new("obkData", individuals=individuals, samples=samples, dna=dna, clinical=clinics)
# plotggMST(x,individualID=42)
# plot huge minimum spanning tree
# plotggMST(x)
# detach(HorseFlu)
