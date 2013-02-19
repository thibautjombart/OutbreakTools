#' Function to plot a minimum spanning tree using ggplot from a the class 'uniqSequences'
#'
#' @param uniqdna an object of the class "uniqSequences"
#' @export
#' @author Joseph Hughes
#' @examples see below

plot.ggMST<-function(uniqdna){
  if (!is.null(uniqdna) && inherits(uniqdna,"uniqSequences")){
    # get the counts for each sequence
    IDcounts<-do.call(rbind, lapply(uniqdna@uniqID, function(x) length(x)))
    IDcounts<-as.data.frame(IDcounts[order(-IDcounts[,1]),])
    colnames(IDcounts) <- c( 'count')
    # print(IDcounts)
    seqindex<-match(rownames(IDcounts), labels(uniqdna@uniqdna))
    # reorder the DNAbin accordingly
    ordereddna<-uniqdna@uniqdna[seqindex, ]
    # print(ordereddna)
    uniqdist<-dist.dna(ordereddna,model="raw", as.matrix=TRUE)
    mstdist<-mst(uniqdist)
    plotcord <- data.frame(gplot.layout.fruchtermanreingold(mstdist, NULL))
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
    eList
    # print(eList)
    # edges of zero are removed
    #emst<-subset(eList,eList[,3]>0)
    edges <- data.frame(plotcord[eList[,1],1:2], plotcord[eList[,2],1:2])

    colnames(edges) <-  c("X1","Y1","X2","Y2")
    # print(edges)

    old <- theme.set(theme.bw())
    old<-theme.update(
               axis.ticks.y = element.blank(), axis.ticks.x = element.blank(),
               axis.title.y = element.blank(),	panel.grid.major.y = element.blank(),
               panel.grid.minor.y = element.blank(), panel.grid.major.x = element.blank(),
               panel.grid.minor.x = element.blank(), panel.border = element.blank(),
               panel.background = element.blank(), legend.position = "none")

    pmst<-ggplot()
    pmst<-pmst+geom.segment(data=edges,aes(x=X1,xend=X2,y=Y1,yend=Y2),lineend="round")
    pmst<-pmst+scale.y.continuous("",labels=NULL)+scale.x.continuous("",labels=NULL)
    pmst<-pmst+geom.point(aes(X1, X2, size=count, colour="red"), data=plotcord)
  }
 # print(pmst)
  return(pmst)
}

##################
####  TESTING ####
##################
## NOTE: THIS MUST BE COMMENTED WHEN COMPILING/INSTALLING THE PACKAGE

## Might want to write a get.dnasubset accessor for this
## get uniq sequences
## Extracting sequenceIDs from the samples table
## seqids<-na.omit(samples[samples$sampleID=="904","sequenceID"])
## get the index of the sequenceIDs in the DNAbin
## seqindex<-which(labels(dna) %in% seqids)
## create a subset DNAbin
## subsetdna<-dna[seqindex, ]
## get a particular sequence id with summary.seq$uniqseqID4[5]
## get number of sequences length(summary.seq$uniqseqID4)
## uniq904<-dna2uniqSequences(subsetdna)
## plot.ggMST(uniq904)

##this below will take a long time
## uniqdna<-dna2uniqSequences(dna)
## plot.ggMST(uniqdna)

