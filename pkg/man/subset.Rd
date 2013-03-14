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
\S4method{subset}{obkData}(x, individuals=NULL, samples=NULL, locus=NULL, sequences=NULL,
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

## USE FAKE DATA IN OBKDATA EXAMPLE ##
data(woodmouse)
dat.dna <- as.list(woodmouse)
ind <- data.frame("individualID"=c("toto","John Doe", "titi"), age=c(20,18,67), sex=c("m","m","?"))
samp <- data.frame(individualID=c('toto','toto','titi'), sampleID=c(1,3,2), date=c("2001-02-13","2001-03-01","2001-05-25"), swab=c("+","-","+"))
samp <- cbind.data.frame(samp, sequenceID=c(1,2,3))
samp$sequenceID <- c("No304","No306","No305")
samp <- samp[c(1,1,2,2,2,3),]
samp$sequenceID <- 1:6
samp$locus <- c("gene1","gene2")[c(1,1,1,2,1,2)]
x <- new("obkData", samples=samp, dna=dat.dna, individuals=ind)


## TEST VARIOUS SUBSETTING ##
## by individual
get.individuals(x)
subset(x, individuals=1)
subset(x, individuals="titi")

## by sample
get.samples(x)
subset(x, sample=2)
subset(x, sample="2")

## by locus
get.locus(x)
subset(x, locus=2)

## by sequences
get.sequences(x)
subset(x, sequences=c("No0909S", "No305"))

## by rows in @individuals
get.data(x,"individuals")
get.data(x, "sex")=="m" # sex is 'm'
subset(x, row.individuals=get.data(x, "sex")=="m") # keep only sex=='m'

## by rows in @samples
get.data(x,"samples")
get.data(x, "swab") == "+" # positive swabs
subset(x, row.samples=get.data(x, "swab") == "+") # keep only positive swabs


## mixing things up (only positive swavs in "toto")
subset(x, row.samples=get.data(x, "swab") == "+", individuals="toto")
}
\keyword{classes}
