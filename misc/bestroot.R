# function to find the best rooting of a tree
# by identifying the rooting that gives the
# highest R^2 between root to tip and sampling time
# sampling times have to be a named vector with
# labels the same as the tree tip labels
#
# horribly inefficient
# Andrew Rambaut's pathogen program, which does
# much the same thing, is far superior, as it
# considers root placement along a branch
# and caches distances (I believe)
bestroot <- function(tr,samptimes){
  # Calculate number of edges and tips
  numedges <- dim(tr$edge)[1]
  numtips <- length(tr$tip.label)
  bestedge <- tr$edge[1,]
  bestr2 <- 0
  r2vec <- rep(0,numedges)
  # Cycle through all edges
  for(i in 1:numedges){
    edgenum <- i
    # Which edge am I?
    thisedge <- tr$edge[edgenum,]
    # I make a new tree rooted at this edge
    if(thisedge[2]>numtips){
      newtree <- tr
      newtree<- root(newtree,node=thisedge[2],resolve.root=T)
      rd <- distRoot(newtree)
      # find corresponding sample times
      idx <- match(names(rd),names(samptimes))
      r2 <- summary(lm(rd~samptimes[idx]))$r.squared
      r2vec[i] <- r2
      if(r2>bestr2){
        bestedge <- thisedge
        bestr2 <- r2
      }
    }
  }
  newtree <- tr
  bestnewtree <- root(newtree,node=bestedge[2],resolve.root=T)
  return(bestnewtree)
}

## set.seed(1)
## tr <- rtree(8)
## samptimes <- runif(8)
## names(samptimes) <- tr$tip.label
## newtr <- bestroot(tr,samptimes)

