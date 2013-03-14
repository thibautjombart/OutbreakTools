% \encoding{UTF-8}
\name{get.dates}

\alias{get.dates}
\alias{get.dates,obkData-method}
\alias{get.dates.obkData}

\title{Access data in "obkData" objects}
\description{
  \code{get.dates} is a generic function with a method for
  \linkS4class{obkData} objects. It can be used to retrieve existing dates from any field of an obkData object.
}
\usage{
get.dates(x, \dots)

\S4method{get.dates}{obkData}(x, data=c("samples", "individuals", "clinical"), \dots)
}
\arguments{
  \item{x}{an \linkS4class{obkData} object.}
  \item{data}{a character string indicating the name of the data field
    to look for. It can be 'samples' (default), 'individuals' or 'clinical'.}
 \item{\dots}{currently not used.}
}
\author{
  Lulla Opatowski
}
\examples{

## LOAD DATA ##
data(FakeInfluenza)
x <- new("obkData", individuals = FakeInfluenza$Patients, samples = FakeInfluenza$Samples, clinical = FakeInfluenza$Clinical, date.format = "\%d/\%m/\%Y")

## VARIOUS USE OF GET.DATES ##
get.dates(x)
get.dates(x, "samples")
get.dates(x, "clinical")

}
\keyword{classes}
