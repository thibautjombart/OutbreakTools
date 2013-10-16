% \encoding{UTF-8}
\name{obkData-class}
\docType{class}
\alias{obkData}
\alias{obkData-class}
\alias{initialize,obkData-method}
\alias{summary,obkData-method}
\alias{summary.obkData}

\alias{show,obkData-method}

\alias{get.locus,obkData-method}
\alias{get.locus.obkData}
\alias{get.nlocus,obkData-method}
\alias{get.nlocus.obkData}

\alias{get.dna,obkData-method}
\alias{get.dna.obkData}

\alias{get.sequences,obkData-method}
\alias{get.sequences.obkData}
\alias{get.nsequences,obkData-method}
\alias{get.nsequences.obkData}

\alias{get.context}
\alias{get.context,obkData-method}
\alias{get.context.obkData}
\alias{get.ncontext}
\alias{get.ncontext,obkData-method}
\alias{get.ncontext.obkData}

\alias{get.records}
\alias{get.records,obkData-method}
\alias{get.records.obkData}
\alias{get.nrecords}
\alias{get.nrecords,obkData-method}
\alias{get.nrecords.obkData}

\alias{get.contacts,obkData-method}
\alias{get.contacts.obkData}
\alias{get.ncontacts,obkData-method}
\alias{get.ncontacts.obkData}

\alias{head.obkData}
\alias{head,obkData-method}

\alias{tail.obkData}
\alias{tail,obkData-method}

\alias{get.trees}
\alias{get.ntrees}
\alias{get.trees,obkData-method}
\alias{get.trees.obkData}
\alias{get.ntrees,obkData-method}
\alias{get.ntrees.obkData}

\title{Formal class "obkData"}
\description{
  The class \code{obkData} is a formal (S4) class for storing data
  collected during outbreaks. This includes:
  \itemize{
    \item individual data (age, sex, onset of symptoms, ...)
    \item time-stamped samples and records (swabs, serology, accession numbers, ...)
    \item genetic sequences
    \item contact information
    \item contextual information
    \item phylogenetic trees
  }
}
\section{Objects from the class obkData}{
  \code{obkData} objects can be created using \code{new("obkData",
    ...)}, where '...' corresponds to the arguments of the corresponding
    initialize method (see 'Usage' section in this page).
}
\section{Slots}{
  \code{obkData} contain the following slots; note that in most cases,
  it is better to retrieve information via accessors (see below), rather
  than by accessing the slots manually. Empty slots are all \code{NULL}.

  \describe{
    \item{\code{individuals}:}{a \code{data.frame} containing individual
      information, with individual labels stored as row names.}

    \item{\code{records}:}{a \code{list} of \code{data.frame}, each
      containing records of a particular type; the first two columns are
      'individualID'and 'date'.}

    \item{\code{contacts}:}{an object of the class
      \linkS4class{obkContacts} storing contact information.}

    \item{\code{dna}:}{an object of the class \linkS4class{obkSequences}
      storing DNA sequences.}

    \item{\code{context}:}{a \code{list} of \code{data.frame}, each
      member of the list containing contextual information related to
      the population of a particular type; the first column is 'date'.}

    \item{\code{trees}:}{an object of the class \code{\link{multiphylo}}
      storing list of trees.}
  }
}

\usage{
\S4method{initialize}{obkData}(.Object, individuals=NULL, records=NULL,
          dna=NULL, trees=NULL, contacts=NULL, context=NULL,
          contacts.start=NULL, contacts.end=NULL,
          contacts.duration=NULL, contacts.directed=FALSE,
          date.format=NULL, dna.individualID=NULL, dna.date=NULL,
          dna.date.format=date.format, dna.sep="_", quiet=FALSE,
          check=TRUE, individuals=NULL, records=NULL, \dots)

get.nrecords(x, \dots)
get.records(x, \dots)
\S4method{get.records}{obkData}(x, \dots)
\S4method{get.nrecords}{obkData}(x, \dots)

get.ncontext(x, \dots)
get.context(x, \dots)
\S4method{get.context}{obkData}(x, \dots)
\S4method{get.ncontext}{obkData}(x, \dots)

get.ntrees(x, \dots)
get.trees(x, \dots)
\S4method{get.ntrees}{obkData}(x, \dots)
\S4method{get.trees}{obkData}(x, \dots)

\S4method{get.dna}{obkData}(x, locus=NULL, id=NULL, \dots)

\S4method{get.ncontacts}{obkData}(x, from=NULL, to=NULL, \dots)
\S4method{get.contacts}{obkData}(x, from=NULL, to=NULL, \dots)

}
\arguments{ \item{.Object}{the prototype of an \code{obkData} object,
  created automatically by \code{new}.}

  \item{individuals}{a data.frame with a mandatory column named
    'individualID', providing unique identifiers for the individuals; if
    missing, row names are used as identifiers.}

  \item{records}{a list of data.frames, each of which has 2 mandatory
    fields, 'individualID' and 'date'. Dates can be specified as
    \code{Date} or \code{characters}, in which case they will be
    converted to dates. Most sensible formats will be detected
    automatically and processed. Unusual formats should be provided
    through the argument \code{date.format}.  Each item of the list
    should be named according to the type of information recorded,
    e.g. 'swabs', 'temperature', or 'hospitalisation' (admission /
    discharge events).}

  \item{dna}{a list matrices of DNA sequences in \code{DNAbin} or
    \code{character} format, each component of the list being a
    different gene. A matrix can be provided if there is a single gene.}

  \item{dna.date}{a vector of collection dates for the DNA sequences;
    see \linkS4class{obkSequences} manpage for more information.}

  \item{dna.individualID}{a vector of individual from which DNA
    sequences where obtained; see \linkS4class{obkSequences} manpage for
    more information.}

  \item{dna.date.format}{a character string indicating the format of the
    date in \code{dna.date} if ambiguous; see \linkS4class{obkSequences}
    manpage for more information.}

  \item{dna.sep}{the character string used to separate fields
    (e.g. sequenceID/individualID/date) in sequences labels; see
    \linkS4class{obkSequences} manpage for more information.}

  \item{contacts}{a matrix of characters indicating contacts using two
    columns; if contacts are directed, the first column is 'from', the
    second is 'to'; values should match individual IDs (as returned by
    get.individuals(x)); if numeric values are provided, these are
    converted to integers and assumed to correspond to individuals
    returned by get.individuals(x).}

  \item{context}{a list of data.frames, each of which has 1 mandatory
    field: 'date'. Each item of the list should be named according to
    the type of information recorded, e.g. 'intervention',
    'vaccination', 'climat' (temperature, humidity, etc.), or schools
    (opening/closure).}

  \item{contacts.start}{a vector of dates indicating the beginning of
    each contact.}

  \item{contacts.end}{a vector of dates indicating the end of each
    contact.}

  \item{contacts.duration}{another way to specify contacts.end, as
    duration of contact in days.}

  \item{contacts.directed}{a logical indicating if contacts are
    directed; defaults to FALSE.}

  \item{trees}{a list of phylogenetic trees with the class
    \code{multiPhylo} (from the ape package)}

  \item{date.format}{a character string indicating the date format (see
    \code{\link{as.Date}}); if \code{NULL}, date format is detected
    automatically, which is usually a sensible option.}

  \item{x}{an \code{obkData} object.}

  \item{locus}{an indication of the locus, either by its name, or using
    integers or logicals matching \code{get.locus}.}

  \item{id}{an indication of the sequences, either by their names, or
    using integers or logicals matching \code{get.sequences}.}

  \item{from,to}{dates indicating the time span to consider for active
    contacts (for dynamic networks only).}

  \item{\dots}{arguments passed to other methods.}

  \item{quiet}{a logical indicating whether informative messages should
    be displayed to the screen.}

  \item{check}{a logical indicating whether supplementary consistency
    checks should be performed.}

}

\section{Methods}{
  Here is a list of accessors available for \code{obkData}
  objects. These functions are used to retrieve the content of the
  object. Specific manpages can exist for the more complex functions.
  These are indicated by a '*' symbol next to the method's name.
  \describe{ \item{show}{\code{signature(x = "obkData")}: printing of
    the object's contents.}
    
    \item{head}{\code{signature(x = "obkData")}: printing of the
      object's contents - showing only the firt lines of each record.}
    
    \item{tail}{\code{signature(x = "obkData")}: printing of the
      object's contents - showing only the last lines of each record.}
    
    \item{summary}{\code{signature(x = "obkData")}: printing a summary
      of the object.}

    \item{get.individuals*}{\code{signature(x = "obkData")}: returns the
      itendifiers of individuals in a given source of data (see manpage
      of \code{\link{get.individuals}} for details).}
    
    \item{get.nindividuals*}{\code{signature(x = "obkData")}: returns
      the number of individuals in a given source of data (see manpage
      of \code{\link{get.individuals}} for details).}

    \item{get.dates*}{\code{signature(x = "obkData")}: returns the dates
      in a given source of data (see manpage of \code{\link{get.dates}}
      for details).}

    \item{get.dates*}{\code{signature(x = "obkData")}: returns the
      number of dates in a given source of data (see manpage of
      \code{\link{get.dates}} for details).}

    \item{get.records}{\code{signature(x = "obkData")}: returns the
      names of existing records tables in the data.}
    
    \item{get.nrecords}{\code{signature(x = "obkData")}: returns the
      number of records tables in the data.}
    
    \item{get.locus}{\code{signature(x = "obkData")}: returns the names
      of the sequenced loci.}
    
    \item{get.nlocus}{\code{signature(x = "obkData")}: returns the
      number of sequenced loci.}
    
    \item{get.nsequences*}{\code{signature(x = "obkData")}: returns the
      number of sequences in the data; see \linkS4class{obkSequences}
      manpage for further details.}

    \item{get.dna*}{\code{signature(x = "obkData")}: returns the
      sequences for a given locus (\code{locus} argument, required if
      more than one locus was sequenced)see \linkS4class{obkSequences}
      manpage for further details.}
    
    \item{get.context}{\code{signature(x = "obkData")}: returns the
      names of existing context tables in the data.}
    
    \item{get.ncontext}{\code{signature(x = "obkData")}: returns the
      number of context tables in the data.}
    
    \item{get.trees}{\code{signature(x = "obkData")}: return a list of
      trees of the class \code{multiPhylo}, if present, and \code{NULL}
      otherwise.}

    \item{get.trees}{\code{signature(x = "obkData")}: return the number
      of trees present in the data.}
    
    \item{get.contacts*}{\code{signature(x = "obkData")}: returns the
      contacts (as \code{network} or \code{networkDynamic}); for dynamic
      contact networks, starting dates can be specified by the argument
      \code{from}, and end date by the argument \code{to}; see
      \linkS4class{obkContacts} manpage for further details.}
    
    \item{get.ncontacts*}{\code{signature(x = "obkData")}: returns the
      number of contacts; for dynamic contact networks, starting dates
      can be specified by the argument \code{from}, and end date by the
      argument \code{to}; see \linkS4class{obkContacts} manpage for
      further details.}
    
    \item{get.data*}{\code{signature(x = "obkData")}: search for a
      matching fields in the object and returns the corresponding
      values; in the absence of match, \code{NULL} is returned. Several
      values can be provided; they can be names of the slots, or any
      variable stored within the data.frames \code{samples} or
      \code{individuals}.  } }
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
## LOAD DATA ##
data(ToyOutbreakRaw)
attach(ToyOutbreakRaw)

## CONSTRUCTING AN OBKDATA OBJECT ##
x <- new ("obkData", individuals=individuals, records=records,
          contacts=contacts, contacts.start=contacts.start,
          contacts.end=contacts.end, dna=dna,
          dna.individualID=dna.info$individualID,
          dna.date=dna.info$date, sample=dna.info$sample, trees=trees)

detach(ToyOutbreakRaw)


## EXAMINING THE OBJECT ##
head(x@individuals)
names(x@records)
lapply(x@records, head)
x@contacts
x@dna
x@trees


## HEAD, TAIL, SUMMARY ##
head(x)
tail(x)
summary(x)


##  ACCESSORS
get.nlocus(x)
get.locus(x)
get.nindividuals(x)
head(get.individuals(x))
get.individuals(x, data="contacts")
get.nsequences(x)
get.dna(x, locus="gene2")
get.dna(x, locus=1)
head(get.data(x, "Fever"))
head(get.data(x, "Age", showSource=TRUE))
head(get.data(x, c("Age","Sex", "infector"), showSource=TRUE))


## GRAPHICS ##
## default plot (timeline of information) ##
plot(x)
plot(x, colorBy='Sex')
plot(x, colorBy='Sex', orderBy='Sex')
plot(subset(x, 1:50), colorBy='Sex', size=4)

## plot contacts ##
plot(x, "contacts", main="Contacts")

\dontrun{
## visualize data on a map ##
plot(x,'geo',location=c('lon','lat'),zoom=15,colorBy='Sex')

## plot the tree ##
plotggphy(x)
plotggphy(subset(x, 1:50), tip.color="Sex", color.pal="Set1")
}
}
\keyword{classes}
