% \encoding{UTF-8}
\name{get.dates}

\alias{get.dates}
\alias{get.ndates}

\alias{get.dates,obkData-method}
\alias{get.dates.obkData}

\alias{get.ndates,obkData-method}
\alias{get.ndates.obkData}

\alias{get.dates,obkSequences-method}
\alias{get.dates.obkSequences}

\alias{get.ndates,obkSequences-method}
\alias{get.ndates.obkSequences}

\alias{get.dates,obkContacts-method}
\alias{get.dates.obkContacts}

\alias{get.ndates,obkContacts-method}
\alias{get.ndates.obkContacts}

\title{Retrieve dates data}
\description{
  \code{get.dates} is a generic function with a method for
  \linkS4class{obkData}, \linkS4class{obkSequences} and
  \linkS4class{obkContacts} objects. It can be used to retrieve
  dates stored in the object.
}
\usage{
get.dates(x, \dots)

get.ndates(x, \dots)

\S4method{get.dates}{obkData}(x, data=c("all", "individuals", "records",
    "dna","context", "contacts"), \dots)

\S4method{get.ndates}{obkData}(x, data=c("all", "individuals", "records",
    "dna","context", "contacts"), \dots)

\S4method{get.dates}{obkSequences}(x, \dots)

\S4method{get.ndates}{obkSequences}(x, \dots)

\S4method{get.dates}{obkContacts}(x, \dots)

\S4method{get.ndates}{obkContacts}(x, \dots)
}
\arguments{
  \item{x}{an input object to seek dates from.}
  \item{data}{a character string indicating the name of the data field
    to look for. It can be 'all'(default), 'individuals', 'records' or
    'context'.}
\item{\dots}{currently not used.}
}
\author{
  Lulla Opatowski, Thibaut Jombart.
}
\examples{

## LOAD DATA ##
data(ToyOutbreak)

## VARIOUS USE OF GET.DATES ##
get.dates(ToyOutbreak)

get.ndates(ToyOutbreak, "records")
get.dates(ToyOutbreak, "records")

get.ndates(ToyOutbreak, "contacts")
get.dates(ToyOutbreak, "contacts")


}
\keyword{classes}
