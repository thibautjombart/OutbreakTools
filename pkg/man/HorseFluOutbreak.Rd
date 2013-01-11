\name{HorseFluOutbreak}
\docType{data}
\alias{HorseFluOutbreak}

\title{Dataset from the Newmarket 2003 equine Influenza outbreak}
\description{
These are the different datasets for the outbreak:
	\itemize{
				\item{
	\code{individuals}: a dataframe which contains all static information relating to a horse (individualID), training yard the horse belongs to (yardID), date of birth, sex, latitude, longitude.
						}
				\item{
	\code{samples}: a dataframe which contains each sample taken from a horse with the date of the sample and the result from the sample. Here, the result from a sample are sequences (sequenceID) and shedding (viral copy number) 
					}
				\item{
	\code{dna}: a DNAbin object alignment of FASTA sequences obtained by cloning and Sanger sequencing (sequenceID, nucleotide sequence)
				}
				\item{
	\code{clinics}: a list of dataframes, there is only one for the date of first vaccination
				}
	}
}
\references{
Hughes J, Allen RC, Baguelin M, Hampson K, Baillie GJ, et al. (2012) Transmission of Equine Influenza Virus during an Outbreak Is Characterized by Frequent Mixed Infections and Loose Transmission Bottlenecks. PLoS Pathog 8(12): e1003081. doi:10.1371/journal.ppat.1003081
}
\keyword{dataset}
