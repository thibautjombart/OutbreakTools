% \encoding{UTF-8}
\name{get.incidence}

\alias{get.incidence}

\alias{get.incidence,Date-method}
\alias{get.incidence.Date}

\alias{get.incidence,obkSequences-method}
\alias{get.incidence.obkSequences}

\alias{get.incidence,obkContacts-method}
\alias{get.incidence.obkContacts}

\alias{get.incidence,obkData-method}
\alias{get.incidence.obkData}

\title{Compute incidence}
\description{
  \code{get.incidence} is a generic function with a method for
  \code{Date}, \linkS4class{obkData}, \linkS4class{obkSequences} and
  \linkS4class{obkContacts} objects. In \linkS4class{obkSequences} and
  \linkS4class{obkContacts} objects, it uses collection/occurence dates
  to derive incidence data. In \linkS4class{obkData}, more options are
  available as different types of information can be used. The procedure
  looks for a time-stamped data with a given named field, and as an
  option allows for selecting specific dates based on the values of that
  field, provided as a range (numeric data), a list of retained values,
  or a regular expression. This allows for defining 'positives' in a
  flexible way.\cr

  In all procedures, one can specify the first and last date, as well as
  the time interval to be used.
}
\usage{
get.incidence(x, \dots)

\S4method{get.incidence}{Date}(x, from=NULL, to=NULL,
          interval=1, add.zero=TRUE, \dots)

\S4method{get.incidence}{obkSequences}(x, from=NULL, to=NULL,
          interval=1, add.zero=TRUE, \dots)

\S4method{get.incidence}{obkContacts}(x, from=NULL, to=NULL,
          interval=1, add.zero=TRUE, \dots)

\S4method{get.incidence}{obkData}(x, data, where=NULL,
          val.min=NULL, val.max=NULL, val.kept=NULL, regexp=NULL,
          from=NULL, to=NULL,
          interval=1, add.zero=TRUE, \dots)

}
\arguments{
  \item{x}{an input object to seek incidence from.}
  \item{from}{the first date to be considered, in \code{Date}
    format, or a character string in the form 'YYYY-mm-dd'
    (e.g. 1982-08-20); if numeric, will be interpreted as number of days
    from the first date.}
  \item{to}{the last date to be considered, in \code{Date}
    format, or a character string in the form 'YYYY-mm-dd'
    (e.g. 1982-08-20); if numeric, will be interpreted as number of days
    from the first date.}
  \item{interval}{the time interval to be used to computed incidence, in
    number of days.}
  \item{add.zero}{a logical indicating if a zero should be added at the
    end of the output.}
  \item{data}{a character string indicating the name of the data field
    to look for, as in \code{\link{get.data}}.}
  \item{where}{an optional character string indicating the where the
    data field should be looked for, as in \code{\link{get.data}}.}
  \item{val.min}{an optional numeric value indicating the minimum value of the
    data to be retained in the incidence (e.g. minimum body temperature).}
  \item{val.max}{an optional numeric value indicating the maximum value of the
    data to be retained in the incidence (e.g. maximum CD4 counts).}
  \item{val.kept}{an optional vector containing all values to be
    retained in the incidence.}
  \item{regexp}{an optional character string providing a regular
    expression to be used to select which data are used in the
    incidence.}
  \item{\dots}{further arguments to be passed to 'grep', for the
    \linkS4class{obkData} procedure.}
}
\value{
  A data.frame with two columns, the first one being dates in the
  \code{Date} format, and the second being integers giving the incidence
  (number of new items for that date).
}
\details{
Incidence is computed for intervals (\[D1;D2\[) which are named after the first
date (D1), including the first date, and excluding the second one (D2).
}
\author{
  Thibaut Jombart \email{t.jombart@imperial.ac.uk}.
}
\examples{

## SIMPLE EXAMPLE ##
dates <- as.Date("2001-01-01") + c(0,1,2,1,1,4)
get.incidence(dates)
get.incidence(dates, interval=2)
get.incidence(dates, interval=2, from="2001-01-03")

## OBKDATA/OBKCONTACTS/OBKSEQUENCES OBJECTS ##
data(ToyOutbreak)

## incidence of DNA sequences collection
get.incidence(ToyOutbreak, "dna")
plot(get.incidence(ToyOutbreak, "dna"), type="s",
     main="Incidence of DNA sequences collection")

## incidence of contacts
get.incidence(ToyOutbreak, "contacts")
plot(get.incidence(ToyOutbreak, "contacts"), type="s",
     main="Incidence of contact occurence")


## incidence of temperature measurements
get.incidence(ToyOutbreak, "temperature")

## same, keeping temperatures above 39
get.incidence(ToyOutbreak, "temperature", val.min=39)
plot(get.incidence(ToyOutbreak, "temperature", val.min=39),
     type="s", main="Incidence: temperature>=39")

## same, temperature>40, interval=2days
inc <- get.incidence(ToyOutbreak, "temperature", val.min=40, interval=2)
plot(inc, type="s", main="Incidence: temperature>=40")

}
\keyword{classes}
