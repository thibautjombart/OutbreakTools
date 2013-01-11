# adapted from code by Lorenzo Milazzo
# function to evaluate delta (measure of the divergence between an
# old sample and the most recent one)
evaluateDelta <- function(no_seq, sampl_scheme,data_stimes, omega) {

  no_s<- length(sampl_scheme)
  no_s_o<- (no_s - 1)
  delta_s<- omega*data_stimes[2:no_s]

  no_seq_o<- (no_seq - sampl_scheme[1])
  delta_seq<- numeric(no_seq_o)
  ds<- 1
  for (i in 1:no_s_o) {
    for (j in 1:sampl_scheme[(i+1)]) {
      delta_seq[ds]<- delta_s[i]
      ds<- ds + 1
    }
  }

  return(delta_seq)

}

# function to define an ID for the old samples (k>1)
# note that ...
#     1(most recent) <= k <= p (oldest)
getIDOldSamples <- function(no_seq, sampl_scheme) {

  no_seq_o<- (no_seq - sampl_scheme[1])
  id_osamples<- numeric(no_seq_o)
  for (i in 1:no_seq_o) {
    id_osamples[i]<- sampl_scheme[1] + i
  }

  return(id_osamples)

}

# function to trim branches from tree
trimBranches <- function(tree, no_seq, sampl_scheme,
                           data_stimes, omega) {

  no_seq_o<- (no_seq - sampl_scheme[1])
  # IDs associated with the tips of the branches to trim
  id_trim<- getIDOldSamples(no_seq, sampl_scheme)

  # delta is associated with the divergence between an
  # old sample and the most recent sample (k=1)
  delta<- evaluateDelta(no_seq, sampl_scheme,
                           data_stimes, omega)

  # trimming the branches
  for (i in 1:no_seq_o) {
    id_edge<- which(tree$edge==id_trim[i])
    if (id_edge>length(tree$edge[,1])) id_edge<- id_edge - length(tree$edge[,1])
    tree$edge.length[id_edge]<- tree$edge.length[id_edge] - delta[i]
  }

  return(tree)

}

# function to generate the corrected distance matrix
generateCorDistMx <- function(no_seq, dists_c) {

  dist_mx_c<- matrix(0, no_seq, no_seq)
  dist_mx_c[lower.tri(dist_mx_c)]<- dists_c
  dist_mx_c_t<- t(dist_mx_c)
  dist_mx_c[upper.tri(dist_mx_c)]<- dist_mx_c_t[upper.tri(dist_mx_c_t)]

  return(dist_mx_c)

}

# we consider data set of `p' samples, with sample `k' obtained more recently
# than sample (k+1) (k=1, 2, ... p)
#         1(most recent) <= n <= m <= p (oldest)
#
# e.g. d(m_i, n_j) is the evolutionary distance between the i-th sequence of
#      the m-th sample and the j-th sequence of the n-th sample;
#
# (note that in the case of isochronous sequence data the ID is always (1,1)
# because all the samplings happen at the same time) 
#
#
# function to evaluate `id_dt'
evaluateIDDistTime = function(no_seq, no_s, sampl_scheme) {

  id_st<- numeric(no_seq)
  j<- 0
  jj<- 0
  for (i in 1:no_s) {
    mult<- sampl_scheme[i]
    jj<- jj+1
    for (i in 1:mult) {
      j<- j+1
      id_st[j]<- jj
    }
  }

  id_dt_3Da<- array(0, dim=c(no_seq, no_seq, 2))
  id_dt_3Da[,,1]<- id_st
  id_dt_3Da[,,2]<- t(id_dt_3Da[,,1])

  id_dt_m<- as.vector(id_dt_3Da[,,1][lower.tri(id_dt_3Da[,,1])])
  id_dt_n<- as.vector(id_dt_3Da[,,2][lower.tri(id_dt_3Da[,,2])])

  id_dt<- cbind(id_dt_m, id_dt_n)

  return(id_dt)

}

# function to evaluate the time intervals between a given sampling
# time and the most recent sampling time
evaluateSampTimesDiff <- function(no_dist_ltmx, no_seq, no_s, 
                                 sampl_scheme, data_stimes){

  # time intervals between sampling times [years]
  stimes_d<- numeric(no_dist_ltmx)
  
  id_dt<- evaluateIDDistTime(no_seq, no_s, sampl_scheme)
    
  for (i in 1:no_dist_ltmx) {
    stimes_d[i]<- data_stimes[id_dt[i,1]] + data_stimes[id_dt[i,2]]
  }
  
  return(stimes_d)

}

evaluateCorDists <- function(dists, no_dist_ltmx,
                               stimes_d, omega) {
  dists_c<- numeric(no_dist_ltmx)
  for (i in 1:no_dist_ltmx) {
    dists_c[i]<- dists[i] + omega*stimes_d[i]
  }

  return(dists_c)

}

#
# in the case of heterochronous sequence data, we introduce an ID associated
# with a given evolutionary distance that takes into account the temporal
# evolution of the sampling procedure;
# this ID (called `id_dt') is defined by a pair (m,n), where `m' and `n' refer
# to two samples in general taken at different times;
#
# we consider data set of `p' samples, with sample `k' obtained more recently
# than sample (k+1) (k=1, 2, ... p)
#         1(most recent) <= n <= m <= p (oldest)
#
# e.g. d(m_i, n_j) is the evolutionary distance between the i-th sequence of
#      the m-th sample and the j-th sequence of the n-th sample;
#
# (note that in the case of isochronous sequence data the ID is always (1,1)
# because all the samplings happen at the same time) 
#
#
# function to evaluate `id_dt'
evaluateIDDistTime <- function(no_seq, no_s, sampl_scheme) {

  id_st<- numeric(no_seq)
  j<- 0
  jj<- 0
  for (i in 1:no_s) {
    mult<- sampl_scheme[i]
    jj<- jj+1
    for (i in 1:mult) {
      j<- j+1
      id_st[j]<- jj
    }
  }

  id_dt_3Da<- array(0, dim=c(no_seq, no_seq, 2))
  id_dt_3Da[,,1]<- id_st
  id_dt_3Da[,,2]<- t(id_dt_3Da[,,1])

  id_dt_m<- as.vector(id_dt_3Da[,,1][lower.tri(id_dt_3Da[,,1])])
  id_dt_n<- as.vector(id_dt_3Da[,,2][lower.tri(id_dt_3Da[,,2])])

  id_dt<- cbind(id_dt_m, id_dt_n)

  return(id_dt)

}


# function to retrieve the pairwise distances `dists' from the
# lower triangular part of the distance matrix;
getDistsLTMx <- function(dist_mx) {

  dists<- as.vector(dist_mx[lower.tri(dist_mx)])

  return(dists)
}

# calculate rates using Jack O'Brien's TREBLE algorithm
# currently broken

calcRate <- function(distances,samplingtimes){
  # check that distances are matrices, and include variance calculation
  if(class(distances)!="matrix") stop("Distances need to be in a matrix")
  if(!("variance"%in%names(attributes(distances)))) stop("Distance matrix needs to include variance estimates")
  p <- attributes(distances)$variance
  seqnames <- attributes(distances)$dimnames[[1]]
  var.dim <- (sqrt(length(p)*8+1)+1)/2
  variance.a <- diag(rep(1,var.dim))
  variance.a[lower.tri(variance.a,diag=FALSE)] <- t(p)
  variance.a <- variance.a+t(variance.a) - diag(diag(variance.a))
  numseq <- dim(distances)[[1]]
  # initialise r and w
  r <- 0
  w <- 0
  for(i in 1:numseq){
    for(j in 1:numseq){
      if(i!=j){
        for(k in 1:numseq){
          if((k!=i)&&(k!=j)&&(variance.a[i*numseq+j]>0)&&(distances[i*numseq+k]>distances[i*numseq+j])&&(distances[j*numseq+k]>distances[i*numseq+j])){
            if(samplingtimes[i]!=samplingtimes[j]){
              rij <- (distances[i*numseq+k]-distances[j*numseq+k])/(samplingtimes[i]-samplingtimes[j])
              wij <- ((samplingtimes[i]-samplingtimes[j])^2)/variance.a[i*numseq+k]
            }
            else{
              rij <- abs(distances[i*numseq+k]-distances[j*numseq+k])*2
              wij <- 0.25/variance.a[i*numseq+k]
            }
            if(rij>0){
              tl <- 0.5*(samplingtimes[i]+samplingtimes[j]-distances[i*numseq+j]/rij)
              te <- 0.25*(samplingtimes[i]+samplingtimes[k]-distances[i*numseq+k]/rij)+0.25*(samplingtimes[k]+samplingtimes[j]-distances[k*numseq+j]/rij)
            }
            print(paste(i,j,k,numseq,samplingtimes[i],samplingtimes[j],samplingtimes[k],rij,wij,tl,te))
            if((rij>0)&&(te<tl)&&(tl<samplingtimes[i])&&(tl<samplingtimes[j])){
              r <- r+rij*wij
              w <- w+wij
            }
          }
        }
      }
    }
  }
  return(r/w)
}

# taken from phangorn, avoided due to its dependency
# not sure that reorderPruning is necessary
upgma <- function(distances) 
{
    # Edited from phangorn
    DD <- as.dist(distances)
    hc <- hclust(DD, method = "average")
    result <- as.phylo(hc)
    # result = reorderPruning(result) # necessary?
    result
}

# The following may be broken
supgma <- function(distances,samplingdates,omega){

  # Number of sequences
  numseq <- dim(distances)[1]

  if(numseq!=length(samplingdates)) stop("Must have one date per sequence")

  # Reorder sequences by date, most recent first
  o <- order(samplingdates,decreasing=TRUE)
  sdates <- samplingdates[o]
  #sq <- sequences[o,]

  # Sampling scheme
  sdatetbl <- rev(table(sdates))
  samplingscheme <- as.integer(sdatetbl)
  
  # Unique sampling times
  # the first time interval is the most recent and it is always equal
  # to zero, the last one is the oldest
  # the time intervals are measured in years

  samplingtimes <- sort(unique(as.double(sdates)))
  samplingtimes <- max(samplingtimes)-samplingtimes
  o <- order(samplingtimes)
  samplingtimes <- samplingtimes[o]
  
  # number of samples
  # (it coincides with the number of time points)
  numsamp <- length(samplingtimes)
  
  # number of elements in the distance matrix
  numdistmx <- numseq^2
  
  # number of elements of the lower triangular part
  # of the distance matrix
  numdistltmx<- (numdistmx-numseq)/2

  stimesd <- evaluateSampTimesDiff(numdistltmx, numseq, numsamp, 
                                   samplingscheme, samplingtimes)
  
  # evaluating the corrected pairwise distances, `distsc'
  distsc<- evaluateCorDists(distances, numdistltmx, stimesd, omega)

  # generating the corrected distance matrix, `distmxc'
  distmxc<- generateCorDistMx(numseq, distsc)

  # performing a UPGMA clustering and generate a UPGMA tree
  tr <- upgma(distmxc)

  trtrim <- trimBranches(tr, numseq, samplingscheme, samplingtimes, omega)

  return(trtrim)
}

## ## tests
## ## load data
## data(singapore)
## ## calculate distances
## dm <- dist.dna(singapore,as.matrix=TRUE,variance=TRUE)
## ## extract sampling dates
## splitheaders <- strsplit(row.names(singapore),":")
## samplingdates <- as.Date(unlist(lapply(splitheaders,"[[",2)),format="%d/%m/%Y")
## samplingtimes <- as.double(samplingdates)
## ## calculate evolutionary rate
## ## do supgma
## omega <- 0.01/365
## stree <- supgma(dm,samplingtimes,omega)
