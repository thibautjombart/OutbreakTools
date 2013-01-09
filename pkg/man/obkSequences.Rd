\name{obkSequences-class}
\docType{class}
\alias{obkSequences}
\alias{obkSequences-class}
\alias{initialize,obkSequences-method}
\alias{show,obkSequences-method}
\alias{get.nlocus,obkSequences-method}
\alias{get.nsequences,obkSequences-method}
\alias{get.locus,obkSequences-method}
% \alias{[,obkSequences-method}
% \alias{[,obkSequences,ANY,ANY-method}
%%%%
\title{Formal class "obkSequences"}
\description{
  The class \code{obkSequences} is a formal (S4) class for storing a DNA
  sequences obtained from a sample during a disease outbreak. Sequences
  from different loci can be stored.

  An \code{obkSequences} object can be constructed from a list of
  sequences (stored as \code{DNAbin} or \code{character} vectors), with
  optional information about loci.
}
\section{Objects from the class obkSequences}{
  \code{obkSequences} objects can be created by calls to \code{new("obkSequences",
    ...)}, where '...' can be the following arguments:
  
  \describe{
    \item{\code{dna}}{a list of DNA sequences in \code{DNAbin} or \code{character} format.}
    \item{\code{locus}}{an optional vector indicating the locus of each
    sequences; its length must match that of the list of sequences.}
  }
}
\section{Slots}{
  The following slots are the content of instances of the class
  \code{obkSequences}; note that in most cases, it is better to retrieve
  information via accessors (see below), rather than by accessing the
  slots manually.
  \describe{
    \item{\code{dna}:}{a list of \code{DNAbin} matrices.}
  }
}
\section{Methods}{
  Here is a list of methods available for \code{obkSequences} objects. Most of
  these methods are accessors, that is, functions which are used to
  retrieve the content of the object. Specific manpages can exist for
  accessors with more than one argument. These are indicated by a '*'
  symbol next to the method's name. This list also contains methods
  for conversion from \code{obkSequences} to other classes.
  \describe{
    \item{show}{\code{signature(x = "obkSequences")}: printing of the
      object.}
    \item{get.nlocus}{\code{signature(x = "obkSequences")}: returns the number of
      loci in the sample.}
    \item{get.nsequences}{\code{signature(x = "obkSequences")}: returns the number of
      sequences in the sample.}
    \item{get.locus}{\code{signature(x = "obkSequences")}: returns the names
      of the loci in the sample.}
    \item{get.dna*}{\code{signature(x = "obkSequences")}: returns the dna
      sequences in the sample for a given locus (\code{locus} argument).}
  }
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{

## THIS IS A TOY EXAMPLE ##
library(ape)
data(woodmouse)

## test constructor / show
new("obkSequences") # empty object
new("obkSequences", woodmouse) # no locus info
new("obkSequences", as.matrix(woodmouse), locus=rep(c('loc1', 'loc2', 'locXX'), c(10,4,1)))


## test accessors
x <- new("obkSequences", as.matrix(woodmouse), locus=rep(c('loc1', 'loc2', 'locXX'), c(10,4,1)))
get.dna(x, locus=1)
get.dna(x, locus="locXX")
get.nlocus(x)
get.nsequences(x)

}
\keyword{classes}
