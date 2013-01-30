\name{dna2uniqSequences}
\alias{dna2uniqSequences}
\title{Function to convert a DNAbin with duplicated sequences to the class 'uniqSequences'}
\usage{
  dna2uniqSequences(DNAbin)
}
\arguments{
  \item{DNAbin}{an object of the class "DNAbin"}

}
\description{
  Function to convert a DNAbin from the class 'DNAbin' to
  the class 'uniqSequences'
}
\examples{
### Might want to write a get.dnasubset accessor for this
### get uniq sequences
### Extracting sequenceIDs from the samples table
data(HorseFluOutbreak)
seqids<-na.omit(samples[samples$sampleID=="904","sequenceID"])
### get the index of the sequenceIDs in the DNAbin
seqindex<-match(seqids,labels(dna))
### create a subset DNAbin
subsetdna<-dna[seqindex, ]
### get a particular sequence id with summary.seq$uniqseqID4[5]
### get number of sequences length(summary.seq$uniqseqID4)
uniqsubset<-dna2uniqSequences(subsetdna)
}
\author{
  Joseph Hughes
}

