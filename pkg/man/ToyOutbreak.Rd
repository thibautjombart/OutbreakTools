\name{ToyOutbreak}
\docType{data}
\alias{ToyOutbreak}

\title{Simulated outbreak dataset}
\description{
  This dataset is a fake dataset containing information on an outbreak amongst 418 individuals. 
	It contains an \code{obkData} object called \code{ToyOutbreak}.
  
  \code{ToyOutbreak@individuals} is a \code{data.frame} containing: 
  \itemize{
    \item{the ID of each individual, stored in the row names of the \code{data.frame},}
    \item{\code{infector}, the ID of the infector of each individual}
    \item{\code{DateInfected}, the date at which each individual was infected}
    \item{\code{Sex}, the sex of each individual}
    \item{\code{Age}, the age of each individual}
    \item{\code{lat}, the latitute corresponding to each individual}
    \item{\code{lon}, the longitude corresponding to each individual}
  }
  
  \code{ToyOutbreak@clinical} is a list containing one \code{data.frame} called \code{Fever} containing: 
  \itemize{
    \item{\code{individualID}, the ID of each individual,}
    \item{\code{date}, the date at which temperature was measured in each individual,}
    \item{\code{temperature}, the temperature measured in each individual.}
  }
  
  \code{ToyOutbreak@contacts} is an \code{obkContacts} object describing the contacts inferred 
  from the first four days of the epidemic tree 
  (it is inferred that a contact occurred between each case and his/her infector within the day preceding infection).   
  
  \code{ToyOutbreak@samples} is a \code{data.frame} containing: 
  \itemize{
    \item{\code{individualID}, the ID of each individual,}
    \item{\code{sampleID}, the ID of blood samples taken from individuals,}
    \item{\code{date}, the date at which each sample was taken,}
    \item{\code{sequenceID}, the ID of dna sequences obtained from the samples,}
    \item{\code{locus}, the loci associated with these sequences.}
  }
  
  \code{ToyOutbreak@dna} is an \code{obkSequences} object storing simulated DNA sequences (for 2 genes, \code{gene1} and \code{gene2}).
 
  \code{ToyOutbreak@tree} is a \code{multiphylo} object storing one phylogenetic tree derived from the concatenated genes. 

}
\examples{

## Load data ##
data(ToyOutbreak)

## Overview of the dataset ##
summary(ToyOutbreak)

## Plotting the dynamic contact network ##
par(mfrow=c(2,2))
plot(ToyOutbreak@contacts,main="Contact network - days 1-4",displaylabels=TRUE)
plot(network.extract(ToyOutbreak@contacts@contacts, onset=10957, terminus=10959), 
	     main="Contact network - days 1-2", displaylabels=TRUE)
plot(network.extract(ToyOutbreak@contacts@contacts, onset=10959, terminus=10960), 
	     main="Contact network - day 3", displaylabels=TRUE)
plot(network.extract(ToyOutbreak@contacts@contacts, onset=10960, terminus=10961), 
	     main="Contact network - day 4", displaylabels=TRUE)

## Mapping the outbreak (by sex) ##
plot(ToyOutbreak,'geo',location=c('lon','lat'),isLonLat=TRUE,zoom=13,colorBy='Sex')

}

\keyword{dataset}
\author{
  Anne Cori, Lulla Opatowski
}
