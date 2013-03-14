\name{obkSequences-class}
\docType{class}
\alias{obkSequences}
\alias{obkSequences-class}
\alias{obkSequencesOrNULL-class}
\alias{initialize,obkSequences-method}
\alias{show,obkSequences-method}

% \alias{get.id}
% \alias{get.id,obkSequences-method}

\alias{get.locus}
\alias{get.locus,obkSequences-method}

\alias{get.nlocus}
\alias{get.nlocus,obkSequences-method}

\alias{get.sequences}
\alias{get.sequences,obkSequences-method}

\alias{get.nsequences}
\alias{get.nsequences,obkSequences-method}

\alias{get.dna}
\alias{get.dna,obkSequences-method}

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
  retrieve the content of the object. Accessors with more than one
  argument are indicated by a '*' symbol next to the method's
  name. This list also contains methods for conversion from
  \code{obkSequences} to other classes.
  
  \describe{
    \item{show}{\code{signature(x = "obkSequences")}: printing of the
      object.}
    \item{get.nlocus}{\code{signature(x = "obkSequences")}: returns the number of
      loci in the sample.}
    \item{get.nsequences}{\code{signature(x = "obkSequences")}: returns the number of
      sequences in the sample; the argument \code{what} can be "total"
    (default), in which case the total number of sequences is returned,
    or "bylocus", in which case the number of sequences per locus is returned.}
    \item{get.locus}{\code{signature(x = "obkSequences")}: returns the names
      of the loci in the sample.}
    \item{get.dna}{\code{signature(x = "obkSequences")}: returns dna
      sequences, which can be optionally specified by locus (argument
    \code{locus}) or by sequence identifier (argument \code{id}); loci
    can be indicated by index or by name (use \code{get.locus} to know
    available loci); sequence IDs must be characters.}
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


## test accessors ##
x <- new("obkSequences", as.matrix(woodmouse), locus=rep(c('loc1', 'loc2', 'locXX'), c(10,4,1)))

## access raw content
get.dna(x)

## access data by locus
get.nlocus(x)
get.locus(x)
get.dna(x, locus=1)
get.dna(x, locus="locXX")
get.dna(x, locus=1:2)
get.nsequences(x)
get.nsequences(x, "bylocus")

## access data by sequence ID
get.dna(x, id=c("No305"))
get.dna(x, id=c("No305","CthulhuDNA"))
get.dna(x, id=c("No305", "No306", "No1114S"))

}
\keyword{classes}
