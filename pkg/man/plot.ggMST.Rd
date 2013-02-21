\name{plot.ggMST}
\alias{plot.ggMST}
\title{Function to plot a minimum spanning tree of the class 'uniqSequences'}
\usage{
  \method{plot}{ggMST}(x, y=NULL, \dots)
}
\arguments{
  \item{x}{An object of the class "uniqSequences"}
  \item{y}{not used - only for compatibility with plot generic}
  \item{\dots}{further arguments to be passed to other functions.}
}
\description{
  Function to plot a minimum spanning tree of a set of unique sequences from \code{uniqSequences} using the mst function from ape 
  and positioning the nodes according to the fruchtermanreingold in sna
}
\examples{
## THIS NEEDS TO BE FIXED ##

% ## Extracting sequenceIDs from the samples table
% seqids<-na.omit(samples[samples$sampleID=="904","sequenceID"])
% ## get the index of the sequenceIDs in the DNAbin
% seqindex<-which(labels(dna) %in% seqids)
% ## create a subset DNAbin
% subsetdna<-dna[seqindex, ]
% ## get a particular sequence id with summary.seq$uniqseqID4[5]
% ## get number of sequences length(summary.seq$uniqseqID4)
% uniq904<-dna2uniqSequences(subsetdna)
% plot_ggMST(uniq904)

% ##this below will take a long time
% uniqdna<-dna2uniqSequences(dna)
% plot_ggMST(uniqdna)

}
\author{
  Joseph Hughes
}

