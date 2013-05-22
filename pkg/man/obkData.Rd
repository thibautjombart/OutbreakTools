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

\alias{get.clinicals}
\alias{get.clinicals,obkData-method}
\alias{get.nclinicals}
\alias{get.nclinicals,obkData-method}

\alias{get.contacts,obkData-method}
\alias{get.ncontacts,obkData-method}

\alias{head.obkData}
\alias{head,obkData-method}

\alias{tail.obkData}
\alias{tail,obkData-method}

\alias{get.trees}
\alias{get.ntrees}
\alias{get.trees,obkData-method}
\alias{get.ntrees,obkData-method}

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
    \item phylogenetic trees
  }
}
\section{Objects from the class obkData}{
  \code{obkData} objects can be created using \code{new("obkData",
    ...)}, where '...' corresponds to the arguments of the corresponding
    initialize method (see 'Usage' section in this page).
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

\usage{
\S4method{initialize}{obkData}(.Object, individuals=NULL, samples=NULL,
          clinical=NULL, dna=NULL, trees=NULL, contacts=NULL,
          contacts.start=NULL, contacts.end=NULL,
          contacts.duration=NULL, contacts.directed=FALSE,
          date.format=NULL, \dots)

get.nindividuals(x, \dots)
get.individuals(x, \dots)

get.nsamples(x, \dots)
get.samples(x, \dots)

get.nclinicals(x, \dots)
get.clinicals(x, \dots)

get.ntrees(x, \dots)
get.trees(x, \dots)

\S4method{get.dna}{obkData}(x, locus=NULL, id=NULL, \dots)

\S4method{get.nindividuals}{obkData}(x, data=c("all", "samples",
"individuals", "clinical", "contacts"), \dots)

\S4method{get.individuals}{obkData}(x, data=c("all", "samples",
"individuals", "clinical", "contacts"), \dots)

\S4method{get.ncontacts}{obkData}(x, from=NULL, to=NULL, \dots)

\S4method{get.contacts}{obkData}(x, from=NULL, to=NULL, \dots)

\S4method{get.clinicals}{obkData}(x, \dots)

\S4method{get.ntrees}{obkData}(x, \dots)

\S4method{get.trees}{obkData}(x, \dots)

}
\arguments{
  \item{.Object}{the prototype of an \code{obkData} object, created
    automatically by \code{new}.}
  \item{individuals}{a data.frame with a mandatory column named
    'individualID', providing unique identifiers for the individuals.}
  \item{samples}{a data.frame with 3 mandatory columns named
    'individualID', 'sampleID', and 'date', providing identifiers for
    the individuals, for the samples, and dates. Dates must be
    provided in a way convertible to \code{Date} (see
    \code{?as.Date}). The format of the dates is detected
    automatically as long as numbers are used, and year is provided
    first or last. Alternative formats can be specified via the
    argument date.format}
  \item{clinical}{a list of data.frames, each of which has 2
    mandatory fields, 'individualID' and 'date'. Dates are specified as
    in the case of the samples data.frame. Each item of the list should
    be named according to the type of clinical event,
    e.g. 'hospitalisation', 'vaccination' or 'symptoms'. The 'date'
    field should be utilised as a start or onset date where the clinical
    event spans a period of time.}
  \item{dna}{a list of DNA sequences in \code{DNAbin} or
    \code{character} format.}
  \item{contacts}{a matrix of characters indicating edges using
    two columns; if contacts are directed, the first column is 'from',
    the second is 'to'; values should match individual IDs (as
    returned by get.individuals(x)); if numeric values are provided,
    these are converted to integers and assumed to correspond to
    individuals returned by get.individuals(x).}
  \item{contacts.start}{a vector of dates indicating the
    beginning of each contact.}
  \item{contacts.end}{a vector of dates indicating the end of
    each contact.}
  \item{contacts.duration}{another way to specify contacts.end,
    as duration of contact in days.}
  \item{contacts.directed}{a logical indicating if contacts are
    directed; defaults to FALSE.}
  \item{trees}{a list of phylogenetic trees in the class
    \code{multiPhylo} (from the ape package)}
  \item{date.format}{a character string indicating the date format (see
    \code{\link{as.Date}}); if \code{NULL}, date format is detected
    automatically.}
  \item{x}{an \code{obkData} object.}
  \item{locus}{an indication of the locus, either by its name, or using
    integers or logicals matching \code{get.locus}.}
  \item{id}{an indication of the sequences, either by their names, or using
    integers or logicals matching \code{get.sequences}.}
  \item{data}{a character string indicating in which slot to seek the
    information; in \code{get.data}, a character string indicating the
    field to be returned.}
  \item{from,to}{dates indicating the time span to consider for active
    contacts (for dynamic networks only).}
  \item{\dots}{arguments passed to other methods.}
}

\section{Methods}{
  Here is a list of accessors available for \code{obkData}
  objects. These functions are used to retrieve the content of the
  object. Specific manpages can exist for the more complex functions.
  These are indicated by a '*' symbol next to the method's name.
  \describe{
    \item{show}{\code{signature(x = "obkData")}: printing of the
      object's contents.}
    \item{head}{\code{signature(x = "obkData")}: printing of the
      object's contents - showing only the firt lines of each record.}
    \item{tail}{\code{signature(x = "obkData")}: printing of the
      object's contents - showing only the last lines of each record.}
    \item{summary}{\code{signature(x = "obkData")}: printing a summary of the
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
 %   \item{get.locus}{\code{signature(x = "obkData")}: returns the names
  %    of the loci in the sample.}
    \item{get.individuals}{\code{signature(x = "obkData")}: returns the
      IDs of individuals in a given source of data (argument
      \code{data}, can be 'all' (default), 'samples', 'individuals', 'clinical', or 'contacts').}
    \item{get.nindividuals}{\code{signature(x = "obkData")}: returns the
      number of individuals in a given source of data (argument
      \code{data}, can be 'all' (default), 'samples', 'individuals', 'clinical', or 'contacts').}
    \item{get.samples}{\code{signature(x = "obkData")}: returns the samples existing in the data.}
    \item{get.nsamples}{\code{signature(x = "obkData")}: returns the number of
      samples in the data.}
    \item{get.clinicals}{\code{signature(x = "obkData")}: returns the names of existing clinical tables in the data.}
    \item{get.nclinicals}{\code{signature(x = "obkData")}: returns the number of clinical tables in the data.}
     \item{get.dates*}{\code{signature(x = "obkData")}: returns the
      dates in a given source of data (argument
      \code{data}, can be 'samples' (default), 'individuals' or 'clinical').}
   \item{get.trees}{\code{signature(x = "obkData")}: return a list of
      trees of the class \code{multiPhylo}, if present, and \code{NULL}
      otherwise.}
    \item{get.contacts*}{\code{signature(x = "obkData")}: returns the
      contacts (as \code{network} or \code{networkDynamic}); for dynamic
      contact networks, starting dates can be specified by the argument
      \code{from}, and end date by the argument \code{to}.}
    \item{get.ncontacts*}{\code{signature(x = "obkData")}: returns the
      number of contacts; for dynamic contact networks, starting dates
      can be specified by the argument \code{from}, and end date by the
      argument \code{to}.}
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
  Cori, Marc Baguelin, Caroline Colijn
}

\seealso{
  \itemize{
    \item \code{\link{subset.obkData}} to subset the data in various ways.
    \item \code{\link{plot.obkData}} to plot the data.
  }
}

\examples{

## LOAD RAW DATA
data(ToyOutbreakRaw)
class(ToyOutbreakRaw)
names(ToyOutbreakRaw)

## MAKE OBKDATA FROM THESE INPUTS
x <- new("obkData", individuals = ToyOutbreakRaw$individuals, samples =
    ToyOutbreakRaw$samples, clinical = ToyOutbreakRaw$clinical,
    contacts=ToyOutbreakRaw$contacts, dna=ToyOutbreakRaw$dna,
    trees=ToyOutbreakRaw$trees)

## HEAD, TAIL, SUMMARY
head(x)
tail(x)
summary(x)

## SOME BASIC PLOTS
plot(x) # time line of samples

x@contacts
plot(x, "contacts", main="Contacts") # contact network

##  ACCESSORS
get.nlocus(x)
get.locus(x)
get.nindividuals(x)
get.individuals(x)
get.individuals(x, data="individuals")
get.nsequences(x)
get.dna(x, locus="gene2")
get.dna(x, locus=1)
head(get.data(x, "samples"))
head(get.data(x, "individuals"))
get.dates(x)
get.dates(x,"samples")
get.nclinicals(x)
get.clinicals(x)
head(get.data(x, "clinical"))

}
\keyword{classes}
