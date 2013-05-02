\name{ToyOutbreakRaw}
\docType{data}
\alias{ToyOutbreakRaw}

\title{Raw simulated outbreak dataset}
\description{
  This dataset is a fake dataset containing information on an outbreak amongst 418 individuals. 
	It contains a list called \code{ToyOutbreakRaw} of the following objects:
  
  - \code{ToyOutbreakRaw$individuals},a \code{data.frame} containing: 
  \itemize{
    \item{the ID of each individual, stored in the row names of the \code{data.frame},}
    \item{\code{infector}, the ID of the infector of each individual}
    \item{\code{DateInfected}, the date at which each individual was infected}
    \item{\code{Sex}, the sex of each individual}
    \item{\code{Age}, the age of each individual}
    \item{\code{lat}, the latitute corresponding to each individual}
    \item{\code{lon}, the longitude corresponding to each individual}
  }
  
  - \code{ToyOutbreakRaw$samples}, a \code{data.frame} containing: 
  \itemize{
    \item{\code{individualID}, the ID of each individual,}
    \item{\code{sampleID}, the ID of blood samples taken from individuals,}
    \item{\code{date}, the date at which each sample was taken,}
    \item{\code{sequenceID}, the ID of dna sequences obtained from the samples,}
    \item{\code{locus}, the loci associated with these sequences.}
  }
  
  - \code{ToyOutbreakRaw$clinical}, a list containing one \code{data.frame} called \code{Fever} containing: 
  \itemize{
    \item{\code{individualID}, the ID of each individual,}
    \item{\code{date}, the date at which temperature was measured in each individual,}
    \item{\code{temperature}, the temperature measured in each individual.}
  }
  
  - \code{ToyOutbreakRaw$contacts}, a \code{matrix} with two columns called \code{from} and \code{to} describing 
  contacts between individuals 
  
  - \code{ToyOutbreakRaw$contacts.start}, a \code{vector} containing the start dates of each contact 
  stored in \code{ToyOutbreakRaw$contacts}. 
  
  - \code{ToyOutbreakRaw$contacts.end}, a \code{vector} containing the end dates of each contact 
  stored in \code{ToyOutbreakRaw$contacts}.
  
  - \code{ToyOutbreakRaw$dna} is a \code{DNAbin} object storing DNA sequences.
 
  - \code{ToyOutbreakRaw$tree} is a \code{multiphylo} object storing a phylogenetic tree. 

}
\examples{
## Load data ##
data(ToyOutbreakRaw)

\dontrun{
## Constructing an obkData object ##
x <- new ("obkData", individuals=ToyOutbreakRaw$individuals,samples=ToyOutbreakRaw$samples,clinical=ToyOutbreakRaw$clinical,contacts=ToyOutbreakRaw$contacts,contacts.start=ToyOutbreakRaw$contacts.start,contacts.end=ToyOutbreakRaw$contacts.end,dna=ToyOutbreakRaw$dna,trees=ToyOutbreakRaw$trees)

## Examining the object ##
summary(x)

head(x@individuals)
head(x@samples)
names(x@clinical)
head(x@clinical$Fever)
x@contacts
x@dna
x@trees

## Plotting the dynamic contact network ##
par(mfrow=c(2,2))
plot(x@contacts,main="Contact network - days 1-4",displaylabels=TRUE)
plot(network.extract(x@contacts@contacts, onset=10957, terminus=10959), 
	     main="Contact network - days 1-2", displaylabels=TRUE)
plot(network.extract(x@contacts@contacts, onset=10959, terminus=10960), 
	     main="Contact network - day 3", displaylabels=TRUE)
plot(network.extract(x@contacts@contacts, onset=10960, terminus=10961), 
	     main="Contact network - day 4", displaylabels=TRUE)

## Mapping the outbreak (by sex) ##
plot(x,'geo',location=c('lon','lat'),isLonLat=TRUE,zoom=13,colorBy='Sex')
}
}
\keyword{dataset}
\author{
  Anne Cori, Lulla Opatowski
}
