\name{plotggMST}
\alias{plotggMST}
\title{Function to plot a minimum spanning tree of the class 'uniqSequences'}
\usage{
  plotggMST(uniqdna)
}
\arguments{
  \item{uniqdna}{An object of the class "uniqSequences"}
}
\description{
  Function to plot a minimum spanning tree of a set of unique sequences from \code{uniqSequences} using the mst function from ape 
  and positioning the nodes according to the fruchtermanreingold in sna
}
\examples{
## load data
data(HorseFlu)
attach(HorseFlu)

## Extracting sequenceIDs from the samples table
seqids<-na.omit(samples[samples$sampleID=="904","sequenceID"])

## get the index of the sequenceIDs in the DNAbin
seqindex<-which(labels(dna) \%in\% seqids)

## create a subset DNAbin
subsetdna<-dna[seqindex, ]

## get a particular sequence id with summary.seq$uniqseqID4[5]
## get number of sequences length(summary.seq$uniqseqID4)
uniq904<-dna2uniqSequences(subsetdna)
plotggMST(uniq904)

\dontrun{
##this below will take a long time
uniqdna<-dna2uniqSequences(dna)
plotggMST(uniqdna)
}

detach(HorseFlu)

}
\author{
  Joseph Hughes
}

