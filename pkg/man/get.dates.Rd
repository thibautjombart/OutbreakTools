% \encoding{UTF-8}
\name{get.dates}

\alias{get.dates}
\alias{get.dates,obkData-method}
\alias{get.dates.obkData}

\title{Access data in "obkData" objects}
\description{
  \code{get.dates} is a generic function with a method for
  \linkS4class{obkData} objects. It can be used to retrieve existing
  dates from any field of an obkData object.
}
\usage{
get.dates(x, \dots)

\S4method{get.dates}{obkData}(x, data=c("all", "samples", "individuals",
       "clinical"), \dots)
}
\arguments{
  \item{x}{an \linkS4class{obkData} object.}
  \item{data}{a character string indicating the name of the data field
    to look for. It can be 'all'(default), 'samples', 'individuals' or 'clinical'.}
 \item{\dots}{currently not used.}
}
\author{
  Lulla Opatowski
}
\examples{

## LOAD DATA ##
data(ToyOutbreak)

## VARIOUS USE OF GET.DATES ##
get.dates(ToyOutbreak)
get.dates(ToyOutbreak, "samples")
get.dates(ToyOutbreak, "clinical")

}
\keyword{classes}
