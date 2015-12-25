\name{ToyOutbreak}
\docType{data}
\alias{ToyOutbreak}

\title{Simulated outbreak dataset}
\description{
  This dataset is a fake dataset containing information on an outbreak
  amongst 418 individuals.  It contains an \code{obkData} object called
  \code{ToyOutbreak}.

  \code{ToyOutbreak@individuals} is a \code{data.frame} containing: 
  \itemize{
    \item{the ID of each individual, stored in the row names of the \code{data.frame}}
    \item{\code{infector}, the ID of the infector of each individual}
    \item{\code{DateInfected}, the date at which each individual was infected}
    \item{\code{Sex}, the sex of each individual}
    \item{\code{Age}, the age of each individual}
    \item{\code{lat}, the latitute corresponding to each individual}
    \item{\code{lon}, the longitude corresponding to each individual}
  }
  
  \code{ToyOutbreak@records} is a list containing one \code{data.frame} called \code{Fever} containing: 
  \itemize{
    \item{\code{individualID}, the ID of each individual,}
    \item{\code{date}, the date at which temperature was measured in each individual,}
    \item{\code{temperature}, the temperature measured in each individual.}
  }
  
  \code{ToyOutbreak@contacts} is an \code{obkContacts} object describing
  the contacts inferred from the first four days of the epidemic tree
  (it is inferred that a contact occurred between each case and his/her
  infector within the day preceding infection).

  \code{ToyOutbreak@dna} is an \code{obkSequences} object storing
  simulated DNA sequences (for 2 genes, \code{gene1} and \code{gene2}).
 
  \code{ToyOutbreak@tree} is a \code{multiphylo} object storing one
  phylogenetic tree derived from the concatenated genes.

}
\examples{

## Load data ##
data(ToyOutbreak)

## Overview of the dataset ##
summary(ToyOutbreak)

## Plotting the dynamic contact network ##
par(mfrow=c(2,2))
plot(get.contacts(ToyOutbreak),main="Contact network - days 0-3",
     displaylabels=TRUE)
plot(get.contacts(ToyOutbreak, from=0, to=1.1),
     main="Contact network - days 0-1", displaylabels=TRUE)
plot(get.contacts(ToyOutbreak, from=2, to=2.1),
     main="Contact network - day 2", displaylabels=TRUE)
plot(get.contacts(ToyOutbreak, from=3, to=3.1),
     main="Contact network - day 3", displaylabels=TRUE)

\dontrun{
## Mapping the outbreak (by sex) ##
plot(ToyOutbreak,'geo',location=c('lon','lat'), zoom=13,
     colorBy='Sex')
}
}
\keyword{dataset}
\author{
  Anne Cori, Lulla Opatowski
}
