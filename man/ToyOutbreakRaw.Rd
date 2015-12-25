\name{ToyOutbreakRaw}
\docType{data}
\alias{ToyOutbreakRaw}

\title{Raw simulated outbreak dataset}
\description{
  This dataset is a fake dataset containing information on an outbreak
  amongst 418 individuals.  It contains a list called
  \code{ToyOutbreakRaw} of the following objects:

  - \code{ToyOutbreakRaw$individuals}, a \code{data.frame} containing:
  \itemize{
    \item{the ID of each individual, stored in the row names of the \code{data.frame}}
    \item{\code{infector}, the ID of the infector of each individual}
    \item{\code{DateInfected}, the date at which each individual was infected}
    \item{\code{Sex}, the sex of each individual}
    \item{\code{Age}, the age of each individual}
    \item{\code{lat}, the latitute corresponding to each individual}
    \item{\code{lon}, the longitude corresponding to each individual}
  }

  - \code{ToyOutbreakRaw$records}, a list containing one
  \code{data.frame} called \code{Fever} containing:
  \itemize{
    \item{\code{individualID}, the ID of each individual,}
    \item{\code{date}, the date at which temperature was measured in each individual,}
    \item{\code{temperature}, the temperature measured in each individual.}
  }
  
  - \code{ToyOutbreakRaw$contacts}, a \code{matrix} with two columns
  called \code{from} and \code{to} describing contacts between
  individuals
  
  - \code{ToyOutbreakRaw$contacts.start}, a \code{vector} containing the
  start dates of each contact stored in \code{ToyOutbreakRaw$contacts}.
  
  - \code{ToyOutbreakRaw$contacts.end}, a \code{vector} containing the
  end dates of each contact stored in \code{ToyOutbreakRaw$contacts}.
  
  - \code{ToyOutbreakRaw$dna} is a list of two \code{DNAbin} matrices
  corresponding to two different genes.

  - \code{ToyOutbreakRaw$dna.info} is a data.frame containing:
   \itemize{
    \item{\code{individualID}, the ID of individuals for each sequence.}
    \item{\code{sampleID}, an indicator of samples from which sequences
      were obtained.}
    \item{\code{date}, the date at samples were collected.}
  }
 
  - \code{ToyOutbreakRaw$tree} is a \code{multiphylo} object storing a
    phylogenetic tree.

}
\examples{
## Load data ##
data(ToyOutbreakRaw)
attach(ToyOutbreakRaw)

## Constructing an obkData object ##
x <- new ("obkData", individuals=individuals, records=records,
          contacts=contacts, contacts.start=contacts.start,
          contacts.end=contacts.end, dna=dna,
          dna.individualID=dna.info$individualID,
          dna.date=dna.info$date, sample=dna.info$sample, trees=trees)

detach(ToyOutbreakRaw)


## Examining the object ##
summary(x)

head(x@individuals)
head(x@records)
names(x@records)
head(x@records$Fever)
x@contacts
x@dna
x@trees

## Plotting the dynamic contact network ##
par(mfrow=c(2,2))
plot(get.contacts(x),main="Contact network - days 0-3",displaylabels=TRUE)
plot(get.contacts(x, from=0, to=1.1), main="Contact network - days 0-1",
     displaylabels=TRUE)
plot(get.contacts(x, from=2, to=2.1), main="Contact network - day 2",
     displaylabels=TRUE)
plot(get.contacts(x, from=3, to=3.1), main="Contact network - day 3",
     displaylabels=TRUE)

## Mapping the outbreak (by sex) ##
plot(x,'geo',location=c('lon','lat'),zoom=13,colorBy='Sex')

}
\keyword{dataset}
\author{
  Anne Cori, Lulla Opatowski
}
