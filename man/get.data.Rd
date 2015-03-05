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

\S4method{get.data}{obkData}(x, data, where=NULL, drop=TRUE,
       showSource=FALSE, \dots)
}
\arguments{
  \item{x}{an \linkS4class{obkData} object.}
  \item{data}{a character string indicating the name of the data field
    to look for.}
  \item{where}{an optional character string indicating the name of the
    slot in which the information should be looked for; if NULL, the
    method will look in all slots starting with \code{@individuals},
    then \code{@samples}, and finally in \code{@records}.}
  \item{drop}{a logical indicating if a vector should be returned
    (TRUE), as opposed to a \code{data.frame} with on single column
    (FALSE).}
  \item{showSource}{a logical indicating if information about
    individualIDs and slot sources should be returned. If TRUE a dataframe
    with three columns will be returned, as opposed to a vector/single
    column if FALSE.}
  \item{\dots}{currently not used.}
}
\author{
  Thibaut Jombart, Lulla Opatowski
}
\examples{

## LOAD DATA ##
data(ToyOutbreak)
ls()


## VARIOUS USE OF GET.DATA ##
# list all the data with name 'Sex' from the obkData object
get.data(ToyOutbreak, "Sex")

# list all the data with 'date' from the obkData object
get.data(ToyOutbreak, "date")
get.data(ToyOutbreak, "date", showSource=TRUE)
get.data(ToyOutbreak, "date", where="records")

# Extract from the obkData object a given field
x <- get.data(ToyOutbreak, "records")
names(x) # x contains the whole 'records' list
head(x$Fever)
x <- get.data(ToyOutbreak, "samples")
names(x) # x contains the whole 'samples' data.frame

}
\keyword{classes}
