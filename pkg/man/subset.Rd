% \encoding{UTF-8}
\name{subset}

\alias{subset,obkData-method}
\alias{subset.obkData}

\title{Subset data in "obkData" objects}
\description{
  \code{subset} is a generic function with a method for
  \linkS4class{obkData} objects. It can be used to subset data by
  specified individuals, samples, loci, sequences, or date range. Note
  that several subsetting criteria can be specified at the same time.
}
\usage{
\S4method{subset}{obkData}(x, individuals=NULL, samples=NULL,
       locus=NULL, sequences=NULL,
       date.from=NULL, date.to=NULL, date.format="",
       row.individuals=NULL, row.samples=NULL, \dots)
}
\arguments{
  \item{x}{an \linkS4class{obkData} object.}
  \item{individuals}{labels of retained individuals; if integer, numeric
    or logical are provided, they are assumed to indicate individuals as
    returned by \code{get.individuals}.}
  \item{samples}{labels of retained samples; if integer, numeric
    or logical are provided, they are assumed to indicate samples as
    returned by \code{get.samples}.}
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
  \item{row.individuals}{rows of \code{x@individuals} to be retained;
    any usual subsetting of a matrix is accepted.}
  \item{row.samples}{rows of \code{x@samples} to be retained;
    any usual subsetting of a matrix is accepted.}
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
subset(x, individuals=1:10)
subset(x, individuals=1)
subset(x, individuals="15")

## by sample
get.samples(x)
subset(x, sample=2)
subset(x, sample="2")

## by locus
get.locus(x)
subset(x, locus=2)

## by sequences
get.sequences(x)
subset(x, sequences=c("gene11", "gene2360"))

## by rows in @individuals
get.data(x,"individuals")
get.data(x, "Sex")=="M" # sex is 'm'
subset(x, row.individuals=get.data(x, "Sex")=="M") # keep only Sex=='M'

## by rows in @samples
get.data(x,"samples")
get.data(x, "date") <= "2000-01-10" # samples with date < Jan 10
subset(x, row.samples=get.data(x, "date") <= "2000-01-10") # keep date < Jan 10


## mixing things up
subset(x, row.samples=get.data(x, "date") <= "2000-01-10", individuals="15")

}
\keyword{classes}
