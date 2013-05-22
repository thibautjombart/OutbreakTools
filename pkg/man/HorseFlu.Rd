\name{HorseFlu}
\docType{data}
\alias{HorseFlu}

\title{Dataset from the Newmarket 2003 equine influenza outbreak}
\description{
  This dataset is an \code{obkData} object with the following components:
  \itemize{
    \item{
      \code{individuals}: a dataframe which contains all static
      information relating to a horse (individualID), training yard the
      horse belongs to (yardID), date of birth, sex, latitude,
      longitude.
    }
    \item{
      \code{samples}: a dataframe which contains each sample taken from
      a horse with the date of the sample and the result from the
      sample. Here, the result from a sample are sequences (sequenceID)
      and shedding (viral copy number)
    }
    \item{
      \code{dna}: a DNAbin object alignment of FASTA sequences obtained
      by cloning and Sanger sequencing (sequenceID, nucleotide sequence)
    }
    \item{
      \code{clinics}: a list of dataframes, there is only one for the
      date of first vaccination
    }
  }
}
\references{
  Hughes J, Allen RC, Baguelin M, Hampson K, Baillie GJ, et al. (2012)
  Transmission of Equine Influenza Virus during an Outbreak Is
  Characterized by Frequent Mixed Infections and Loose Transmission
  Bottlenecks. PLoS Pathog 8(12):
  e1003081. doi:10.1371/journal.ppat.1003081
}
\examples{
\dontrun{

## LOAD DATA ##
data(HorseFlu)

## EXAMINE CONTENT ##
summary(HorseFlu)

## individual info
head(HorseFlu@individuals)

## sample info
head(HorseFlu@samples)

## DNA sequences
HorseFlu@dna

## clinical info
names(HorseFlu@clinical)
head(HorseFlu@clinical$FirstVac)
head(HorseFlu@clinical$LastVac)

## MANIPULATE OBJECT ##

## How many individuals, samples and sequences?
get.nindividuals(HorseFlu)
get.nsamples(HorseFlu)
get.nsequences(HorseFlu)

## How many sequences per individual?
table(HorseFlu@samples$individualID)
barplot(table(HorseFlu@samples$individualID), horiz=TRUE, las=1,
       xlab="number of samples", cex.names=.8)

## Identify most sampled individual
ind.max <- names(which.max(table(HorseFlu@samples$individualID)))
HorseFlu.max <- subset(HorseFlu, individual=ind.max)
get.individuals(HorseFlu.max)

## How many samples and sequences for this individual?
get.samples(HorseFlu.max)
table(HorseFlu.max@samples$sampleID)

## Build a phylogeny for this individual
get.phylo(HorseFlu.max, type="fan")

}
}
\keyword{dataset}
