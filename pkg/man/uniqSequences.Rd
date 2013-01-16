\name{uniqSequences-class}
\docType{class}
\alias{uniqSequences}
\alias{uniqSequences-class}
\title{Formal class "uniqSequences"}
\description{
  The class \code{uniqSequences} is a formal (S4) class for storing unique sequences in the form
  of a \code{DNAbin} and an associated list of vectors containing the original sequenceID
}
\section{Objects from the class uniqSequences}{
  \code{uniqSequences} objects can be created using \code{new("uniqSequences",uniqID="listOrNULL", uniqdna="DNAbin")}:
  \describe{
    \item{\code{uniqdna}}{an object of type DNAbin.}
    \item{\code{uniqID}}{a list of vectors with the vector names corresponding to the labels of DNAbin.}
  }
}
\section{Slots}{
  \code{uniqSequences} contain the following slots.
  \describe{
    \item{\code{uniqdna}:}{a \code{DNAbin} containing the unique sequences.}
    \item{\code{uniqID}:}{a \code{list} containing the unique IDs for each sequence and the 
     names of the original sequences as a vector.}
  }
}

\author{
  Joseph Hughes... add your name/email here}
\examples{

## EMPTY OBJECT ##
new("uniqSequences")

data(HorseFluOutbreak)
# get some sequence IDs from sample
seqids<-na.omit(samples[samples$sampleID=="904","sequenceID"])
# get the index of the sequenceIDs in the DNAbin
seqindex<-which(labels(dna) %in% seqids)
# create a subset DNAbin
subsetdna<-dna[seqindex, ]
# see the function dna2uniqSequences
uniqsubset<-dna2uniqSequences(subsetdna)

}
\keyword{classes}
