\name{dna2uniqSequences}
\alias{dna2uniqSequences}
\title{
  Convert a DNAbin with duplicated sequences to the class 'uniqSequences'}
\usage{
  dna2uniqSequences(dna)
}
\arguments{
  \item{dna}{an object of the class "DNAbin"}

}
\description{
  Function to convert a DNAbin from the class 'DNAbin' to
  the class 'uniqSequences'
}
\examples{
### get uniq sequences for individual 49

data(HorseFlu)
x <- HorseFlu

### create a subset DNAbin
id49 <- subset(x,individuals="49") # warning message is returned
dna49 <- get.dna(id49)[[1]]

### get a particular sequence id with summary.seq$uniqseqID4[5]
### get number of sequences length(summary.seq$uniqseqID4)
uniqdna49 <- dna2uniqSequences(dna49)

}
\author{
  Joseph Hughes
}

