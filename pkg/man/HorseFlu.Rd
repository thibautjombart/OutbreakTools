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
      \code{dna}: an \linkS4class{obkSequences} object containing DNA
      sequences and their meta-information.
    }
    \item{
      \code{records}: a list of dataframes containing information about
      the dates of first and last vaccinations, and shedding data
      (viral copy number) for some samples.
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

## DNA sequences
HorseFlu@dna

## records info
lapply(HorseFlu@records, head)


## How many individuals and sequences?
get.nindividuals(HorseFlu)
get.nsequences(HorseFlu)

## How many sequences per individual?
ind <- table(get.data(HorseFlu, "individualID", where="dna"))
ind
barplot(sort(ind), horiz=TRUE, las=1,
       xlab="number of samples", cex.names=.8)

## How many sequences for this individual?
ind.42 <- subset(HorseFlu, individualID="42")
get.nsequences(ind.42)

## How many samples?
length(unique(get.data(HorseFlu, "sampleID", where="dna")))

## How many sequences per sample?
table(get.data(HorseFlu, "sampleID", where="dna"))


}
}
\keyword{dataset}
