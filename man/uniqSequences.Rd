\name{uniqSequences-class}
\docType{class}
\alias{uniqSequences}
\alias{uniqSequences-class}
\alias{initialize,uniqSequences-method}
\title{Formal class "uniqSequences"}
\description{
  The class \code{uniqSequences} is a formal (S4) class for
  storing unique sequences in the form of a \code{DNAbin} and an
  associated list of vectors containing the original sequenceID.
}
\section{Objects from the class uniqSequences}{
  \code{uniqSequences} objects can be created using
  \code{new("uniqSequences",uniqID="listOrNULL", uniqdna="DNAbin")}:
  \describe{
    \item{\code{uniqdna}}{an object of type DNAbin.}
    \item{\code{uniqID}}{a list of vectors with the vector names corresponding to the labels of DNAbin.}
  }
}
\section{Slots}{
  \code{uniqSequences} contain the following slots.
  \describe{
    \item{\code{uniqdna}:}{a \code{DNAbin} containing the unique sequences.}
    \item{\code{uniqID}:}{a \code{list} containing the unique IDs for
      each sequence and the names of the original sequences as a vector.}
  }
}

\author{
  Joseph Hughes
}
\examples{

## EMPTY OBJECT
new("uniqSequences")

## LOAD RAW DATA
data(HorseFlu)

## GET SEQUENCES FROM THE FIRST 3 INDIVIDUALS
## all sequences
dna <- get.dna(subset(HorseFlu, individuals=1:3))[[1]]
dna

if(require(ape)){
plot(nj(dist.dna(dna)), type="unr")
title("NJ tree - all sequences")

## only unique sequences
dna2 <- dna2uniqSequences(dna)
dna2
plot(nj(dist.dna(dna2@uniqdna)), type="unr")
title("NJ tree - only unique sequences")

}
}
\keyword{classes}
