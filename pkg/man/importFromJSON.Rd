\encoding{UTF-8}
\name{JSON2obkData}
\alias{JSON2obkData}
\title{Import data from JSON file into OutbreakTools}
\description{
  \code{JSON2obkData} imports outbreak data stored in one or several
  JSON files and converts it into an \linkS4class{obkData} object.

  ! This feature is currently experimental ! 
}
\usage{
 JSON2obkData(individuals=NULL, records=NULL, contacts=NULL, context=NULL)
}
\arguments{
  \item{individuals}{a character string containing data on individuals in JSON format}
  \item{records}{a character string containing data on records and samples in JSON format}
  \item{contacts}{a character string containing data on contacts in JSON format}
  \item{context}{a character string containing data on context in JSON format}

}
\value{
  An \linkS4class{obkData} object.
}
\author{
 Thibaut Jombart \email{t.jombart@imperial.ac.uk}
}
