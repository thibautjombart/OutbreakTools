% \encoding{UTF-8}
\name{obkData-class}
\docType{class}
\alias{obkData}
\alias{obkData-class}
\alias{initialize,obkData-method}
\alias{summary,obkData-method}

\alias{show,obkData-method}
\alias{get.locus,obkData-method}
\alias{get.nlocus,obkData-method}

\alias{get.dna,obkData-method}
\alias{get.sequences,obkData-method}
\alias{get.nsequences,obkData-method}

\alias{get.individuals}
\alias{get.individuals,obkData-method}
\alias{get.nindividuals}
\alias{get.nindividuals,obkData-method}

\alias{get.samples}
\alias{get.samples,obkData-method}
\alias{get.nsamples}
\alias{get.nsamples,obkData-method}

% \alias{get.data}
% \alias{get.data,obkData-method}

\alias{get.trees}
\alias{get.trees,obkData-method}


% \alias{[,obkData-method}
% \alias{[,obkData,ANY,ANY-method}
%%%%
\title{Formal class "obkData"}
\description{
  The class \code{obkData} is a formal (S4) class for storing data
  collected during outbreaks. This includes:
  \itemize{
    \item individual data (age, sex, onset of symptoms, ...)
    \item sample data (swabs, serology, accession numbers, ...)
    \item genetic sequences
    \item contact information
    \item clinical data
  }
}
\section{Objects from the class obkData}{
  \code{obkData} objects can be created using \code{new("obkData",
    ...)}, where '...' can be the following optional arguments (defaults
  are all \code{NULL}):
  \describe{
    \item{\code{individuals}}{a data.frame with a mandatory column named
      'individualID', providing unique identifiers for the individuals.}
    \item{\code{samples}}{a data.frame with 3 mandatory columns named
      'individualID', 'sampleID', and 'date', providing identifiers for
      the individuals, for the samples, and dates. Dates must be
      provided in a way convertible to \code{Date} (see
      \code{?as.Date}). Default format for dates if in character strings
      is "%Y-%m-%d". Alternative format can be specified via the
      argument date.format}
    \item{\code{clinical}}{a list of data.frames, each of which has 2 mandatory fields, 'individualID' and 'date'. Dates are specified as in the case of the samples data.frame. Each item of the list should be named according to the type of clinical event, e.g. 'hospitalisation', 'vaccination' or 'symptoms'. The 'date' field should be utilised as a start or onset date where the clinical event spans a period of time.}
    \item{\code{dna}}{a list of DNA sequences in \code{DNAbin} or
      \code{character} format.}
    \item{\code{contacts}}{... - to be filled}
    \item{\code{trees}}{a list of phylogenetic trees in the class
      \code{multiPhylo} (from the ape package)}
  }
}
\section{Slots}{
  \code{obkData} contain the following slots; note that in most cases, it is better to retrieve
  information via accessors (see below), rather than by accessing the
  slots manually. Empty slots are all \code{NULL}.
  \describe{
    \item{\code{individuals}:}{a \code{data.frame} containing individual
      information, with individual labels stored as row names.}
    \item{\code{samples}:}{a \code{data.frame} containing sample
      information; the first three columns are 'individualID',
      'sampleID', and 'date'.}
    \item{\code{clinical}:}{a \code{list} of \code{data.frame}, each
      member of the list containing clinical information of a particular
      type; the first two columns are 'individualID'and 'date'.}
    \item{\code{contacts}:}{an object of the class
      \linkS4class{obkContacts} storing contact information.}
    \item{\code{dna}:}{an object of the class
      \linkS4class{obkSequences} storing DNA sequences.}
    \item{\code{trees}:}{an object of the class
      \code{\link{multiphylo}} storing list of trees.}
  }
}
\section{Methods}{
  Here is a list of accessors available for \code{obkData}
  objects. These functions are used to retrieve the content of the
  object. Specific manpages can exist for the more complex functions.
  These are indicated by a '*' symbol next to the method's name.
  \describe{
    \item{show}{\code{signature(x = "obkData")}: printing of the
      object.}
    \item{get.locus}{\code{signature(x = "obkData")}: returns the loci
      existing in the data.}
    \item{get.nlocus}{\code{signature(x = "obkData")}: returns the number of
      loci.}
    \item{get.dna*}{\code{signature(x = "obkData")}: returns the
      sequences for a given locus (\code{locus} argument, required if
      more than one locus was sequenced).}
    \item{get.nsequences}{\code{signature(x = "obkData")}: returns the number of
      sequences in the data.}
    \item{get.locus}{\code{signature(x = "obkData")}: returns the names
      of the loci in the sample.}
    \item{get.individuals*}{\code{signature(x = "obkData")}: returns the
      IDs of individuals in a given source of data (argument
      \code{data}, can be 'samples' (default) or 'individuals').}
    \item{get.nindividuals*}{\code{signature(x = "obkData")}: returns the
      number of individuals in a given source of data (argument
      \code{data}, can be 'samples' (default) or 'individuals').}
    \item{get.trees}{\code{signature(x = "obkData")}: return a list of
      trees of the class \code{multiPhylo}, if present, and \code{NULL}
      otherwise.}
    \item{get.data*}{\code{signature(x = "obkData")}: search for a
      matching fields in the object and returns the corresponding
      values; in the absence of match, \code{NULL} is returned. Several
      values can be provided; they can be names of the slots, or any
      variable stored within the data.frames \code{samples} or
      \code{individuals}.
    }
  }
}
\author{
  Thibaut Jombart, Simon Frost, Lulla Opatowski, Paul Birrell, Anne
  Cori, Marc Baguelin, Caroline Colijn... add your name/email here}
\examples{

## EMPTY OBJECT ##
new("obkData")

## INDIVIDUAL INFO ONLY ##
new("obkData", individuals=data.frame("individualID"=letters))
new("obkData", individuals=data.frame("individualID"=letters, age=1:26, 1:26))


samp <- data.frame(individualID=c('toto','toto','titi'), sampleID=c(1,3,2), date=c("2001-02-13","2001-03-01","2001-05-25"), swab=c("+","-","+"))


## SAMPLE INFO ONLY ##
new("obkData", sample=samp)
new("obkData", sample=samp[,c(1:3)] )
new("obkData", sample=samp[,c(1:3,4,4,4)] )

## SAMPLE & INDIV INFO - MISSING INDIV ##
new("obkData", sample=samp[,c(1:3,4,4,4)] , individuals=data.frame("individualID"=letters, age=1:26))

## SAMPLE & INDIV INFO ##
ind <- data.frame("individualID"=c("toto","John Doe", "titi"), age=c(20,18,67), sex=c("m","m","?"))
new("obkData", sample=samp, ind=ind)


## DNA INFO, NOTHING ELSE ##
library(ape)
data(woodmouse)
dat.dna <- as.list(woodmouse)

new("obkData", dna=dat.dna) # should be empty

## SAMP + DNA INFO ##
samp <- data.frame(individualID=c('toto','toto','titi'), sampleID=c(1,3,2), date=c("2001-02-13","2001-03-01","2001-05-25"), swab=c("+","-","+"))

samp <- cbind.data.frame(samp, sequenceID=c(1,2,3))

## sequences given as indices
new("obkData", samples=samp, dna=dat.dna) # (note the nice sample ordering)

## sequences given as IDs
samp$sequenceID <- c("No304","No306","No305")
new("obkData", samples=samp, dna=dat.dna) # (note the nice sample ordering)

## sequences given as IDs, with wrong IDs
## samp$sequenceID <- c("No304","No306","Arrrhhh") # this would generate a meaningful error
## new("obkData", samples=samp, dna=dat.dna) # (note the nice sample ordering)


## multiple sequences per individual
samp$sequenceID <- c("No304","No306","No305")
samp <- samp[c(1,1,2,2,2,3),]
samp$sequenceID <- 1:6
new("obkData", samples=samp, dna=dat.dna)


## multiple sequences per individual, locus information
samp$locus <- c("gene1","gene2")[c(1,1,1,2,1,2)]
new("obkData", samples=samp, dna=dat.dna)

## incorporating data on sample and clinical data
data(FakeInfluenza)
new("obkData", individuals = FakeInfluenza$Patients, samples = FakeInfluenza$Samples, clinical = FakeInfluenza$Clinical, date.format = "\%d/\%m/\%Y")


## TEST ACCESSORS ##
x <- new("obkData", samples=samp, dna=dat.dna, individuals=ind)

get.locus(x)
get.individuals(x)
get.individuals(x, "individuals")
get.dna(x, locus="gene2")
get.nsequences(x)
get.data(x, "samples")
get.data(x, "individuals")
get.data(x, c("date","swab"))
table(get.data(x, c("swab", "locus")))

}
\keyword{classes}
