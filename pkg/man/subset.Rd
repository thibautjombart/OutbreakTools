% \encoding{UTF-8}
\name{subset}

\alias{subset,obkData-method}
\alias{subset.obkData}

\alias{subset,obkSequences-method}
\alias{subset.obkSequences}

\alias{subset,obkContacts-method}
\alias{subset.obkContacts}

\title{Subset data in "obkData" objects}

\description{ \code{subset} is a generic function with methods for
  \linkS4class{obkData}, \linkS4class{obkSequences} and
  \linkS4class{obkContacts} objects. It can be used to subset data by
  specified individuals, samples, loci, sequences, or date range. Note
  that several subsetting criteria can be specified at the same time.
}
\usage{
\S4method{subset}{obkData}(x, individuals=NULL, locus=NULL, sequences=NULL,
       date.from=NULL, date.to=NULL, date.format=NULL, \dots)

\S4method{subset}{obkSequences}(x, sequences=NULL, locus=NULL, individuals=NULL,
       date.from=NULL, date.to=NULL, date.format=NULL, \dots)

\S4method{subset}{obkContacts}(x, individuals=NULL, date.from=NULL,
        date.to=NULL, date.format=NULL, \dots)
}
\arguments{
  \item{x}{an \linkS4class{obkData} object.}
  
  \item{individuals}{labels of retained individuals; if integer, numeric
    or logical are provided, they are assumed to indicate individuals as
    returned by \code{get.individuals}.}

  \item{locus}{labels of retained loci; if integer, numeric
    or logical are provided, they are assumed to indicate locus as
    returned by \code{get.locus}.}
  
  \item{sequences}{labels of retained loci; if integer, numeric
    or logical are provided, they are assumed to indicate sequences as
    returned by \code{get.sequences}.}
  
  \item{date.from, date.to}{the range of dates (in Date format) of
    retained samples.}
  
  \item{date.format}{the format for dates if in character strings;
    defaults to "\%Y-\%m-\%d" (see \code{?as.Date}).}

  \item{\dots}{currently not used.}
}
\author{
  Thibaut Jombart
}
\examples{

## LOAD ToyOutbreak obkData object ##
data(ToyOutbreak)
x <- ToyOutbreak


## TEST VARIOUS SUBSETTING ##
## by individual
get.individuals(x)
plot(x)
plot(subset(x, individuals=1:10))
subset(x, individuals=1)
subset(x, individuals="15")

## by locus
get.locus(x)
subset(x, locus=2)

## by locus and sequences
subset(x, locus=1, sequence=1:10)
plot(subset(x, locus=1, sequence=1:10))
plot(subset(x, locus=1, sequence=1:10), "phy")

## by dates
get.dates(x)
plotIndividualTimeline(subset(x, date.to="2000-01-05"), "dna")

}
\keyword{classes}
