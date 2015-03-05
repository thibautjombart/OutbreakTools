\name{HorseFluRaw}
\docType{data}
\alias{HorseFluRaw}

\title{Raw dataset from the Newmarket 2003 equine influenza outbreak}
\description{
  This dataset is a list containing the following components:
  \itemize{
    \item{
      \code{dna}: a DNAbin object alignment of FASTA sequences obtained
      by cloning and Sanger sequencing (sequenceID, nucleotide
      sequence); sequence labels contain sequence identifiers,
      individual IDs, and dates, separated by "_".
    }
    \item{
      \code{individuals}: a dataframe which contains all static
      information relating to a horse (individualID), training yard the
      horse belongs to (yardID), date of birth, sex, latitude,
      longitude.
    }
    \item{
      \code{records}: a list of dataframes containing information about
      the dates of first and last vaccinations, and shedding data
      (viral copy number) for some samples.
    }
    \item{
      \code{dna.info}: an indication of the samples from which the DNA
      sequences were obtained.
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
data(HorseFluRaw)
attach(HorseFluRaw)

## EXAMINE CONTENT ##

## individual info
head(individuals)

## records info
lapply(records, head)

## DNA sequences
dna

## CREATE OBKDATA ##
x <- new("obkData", individuals=individuals, records=records,
         dna=dna, sampleID=dna.info$sampleID)


## MANIPULATE OBJECT ##

## How many individuals and sequences?
get.nindividuals(x)
get.nsequences(x)

## How many sequences per individual?
ind <- table(get.data(x, "individualID", where="dna"))
ind
barplot(sort(ind), horiz=TRUE, las=1,
       xlab="number of samples", cex.names=.8)

## How many sequences for this individual?
ind.42 <- subset(x, individualID="42")
get.nsequences(ind.42)

## How many samples?
length(unique(get.data(x, "sampleID", where="dna")))

## How many sequences per sample?
table(get.data(x, "sampleID", where="dna"))

detach(HorseFluRaw)


}
}
\keyword{dataset}
