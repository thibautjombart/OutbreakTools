% \encoding{UTF-8}
\name{get.data}

\alias{get.data}
\alias{get.data,obkData-method}
\alias{get.data.obkData}

\title{Access data in "obkData" objects}
\description{
  \code{get.data} is a generic function with a method for
  \linkS4class{obkData} objects. It can be used to retrieve various
  information, known only by the name of the field looked for.
}
\usage{
get.data(x, \dots)

\S4method{get.data}{obkData}(x, data, where=NULL, drop=TRUE, showSource=FALSE, \dots)
}
\arguments{
  \item{x}{an \linkS4class{obkData} object.}
  \item{data}{a character string indicating the name of the data field
    to look for.}
  \item{where}{an optional character string indicating the name of the
    slot in which the information should be looked for; if NULL, the
    method will look in all slots starting with \code{@individuals}, then \code{@samples},
    and finally in \code{@clinical}.}
  \item{drop}{a logical indicating if a vector should be returned
    (TRUE), as opposed to a \code{data.frame} with on single column
    (FALSE).}
  \item{showSource}{a logical indicating if information about individualIDs and slot sources should be returned. If TRUE a dataframe with three columns will be returned, as opposed to a vector/single column if FALSE.}
  \item{\dots}{currently not used.}
}
\author{
  Thibaut Jombart, Lulla Opatowski
}
\examples{

## LOAD DATA ##
data(FakeInfluenza)
x <- new("obkData", individuals = FakeInfluenza$Patients, samples = FakeInfluenza$Samples, clinical = FakeInfluenza$Clinical, date.format = "\%d/\%m/\%Y")

## VARIOUS USE OF GET.DATA ##
get.data(x, "gender")
get.data(x, "gender", drop=FALSE)
get.data(x, "gender", showSource=TRUE)

get.data(x, "date")
get.data(x, "date", showSource=TRUE)
get.data(x, "date", where="clinical")

get.data(x, "clinical")
get.data(x, "samples")
}
\keyword{classes}
