% \encoding{UTF-8}
\name{get.individuals}

\alias{get.individuals}
\alias{get.nindividuals}

\alias{get.individuals,obkData-method}
\alias{get.individuals.obkData}

\alias{get.nindividuals,obkData-method}
\alias{get.nindividuals.obkData}

\alias{get.individuals,obkSequences-method}
\alias{get.individuals.obkSequences}

\alias{get.nindividuals,obkSequences-method}
\alias{get.nindividuals.obkSequences}

\alias{get.individuals,obkContacts-method}
\alias{get.individuals.obkContacts}

\alias{get.nindividuals,obkContacts-method}
\alias{get.nindividuals.obkContacts}

\title{Retrieve individual identifiers}
\description{
  \code{get.individuals} is a generic function with a method for
  \linkS4class{obkData}, \linkS4class{obkSequences} and
  \linkS4class{obkContacts} objects. It can be used to retrieve
  individuals identifiers stored in the object.
}
\usage{
get.individuals(x, \dots)

get.nindividuals(x, \dots)

\S4method{get.individuals}{obkData}(x,
    data=c("all", "individuals", "records", "contacts", "dna"), \dots)

\S4method{get.nindividuals}{obkData}(x,
    data=c("all", "individuals", "records", "contacts", "dna"), \dots)

\S4method{get.individuals}{obkSequences}(x, \dots)

\S4method{get.nindividuals}{obkSequences}(x, \dots)

\S4method{get.individuals}{obkContacts}(x, \dots)

\S4method{get.nindividuals}{obkContacts}(x, \dots)
}
\arguments{
  \item{x}{an input object to seek individuals from.}
  \item{data}{a character string indicating the name of the data field
    to look for. It can be 'all'(default), 'individuals', 'records' or
    'context'.}
\item{\dots}{currently not used.}
}
\author{
  Thibaut Jombart, Lulla Opatowski, Simon Frost.
}
\examples{

## LOAD DATA ##
data(ToyOutbreak)

## VARIOUS USE OF GET.INDIVIDUALS ##
get.individuals(ToyOutbreak)

get.nindividuals(ToyOutbreak, "records")
get.individuals(ToyOutbreak, "records")

get.nindividuals(ToyOutbreak, "contacts")
get.individuals(ToyOutbreak, "contacts")


}
\keyword{classes}
