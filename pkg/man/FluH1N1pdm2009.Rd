\docType{data}
\name{FluH1N1pdm2009}
\alias{FluH1N1pdm2009}
\title{Dataset from the 2009 influenza A/H1N1 pandemic}
\description{
  This dataset is a \code{list} containing the following
  objects: \enumerate{ \item\code{individuals}: a data
  frame containing 514 individual IDs as well as their
  locations. \item\code{samples}: a data frame containing
  514 individual IDs, their sample IDs and dates as well as
  the IDs of the associated genetic sequences.
  \item\code{dna}: a \code{\link{DNAbin}} object containing
  514 genetic sequences of influenza A/H1N1/2009
  haemagglutinin (HA). \item\code{tree}: a
  \code{\link{multiphylo}} object containing the maximum
  clade credibility (MCC) tree obtained via the Beast
  analysis of the 514 genetic sequences. }
}
\examples{
## load the dataset
data(FluH1N1pdm2009)
attach(FluH1N1pdm2009)

head(individuals)
head(samples)


## create obkData object
x <- new("obkData", individuals = individuals, dna = FluH1N1pdm2009$dna,
      dna.individualID = samples$individualID, dna.date = samples$date,
      trees = FluH1N1pdm2009$trees)

## have a look at the summary
summary(x)

## plot the phylogeny
plotggphy(x, tip.color="location")

p <- plotggphy(x, ladderize = TRUE, branch.unit = "year")
p

detach(FluH1N1pdm2009)


}
\author{
  Anton Camacho
}
\references{
  This dataset is part of Trevor Bedford's tutorial on the
  Beast software: \emph{Inferring spatiotemporal dynamics
  of the H1N1 influenza pandemic from sequence data},
  available at
  \url{https://github.com/trvrb/influenza-dynamics-practical}.
}
\keyword{datasets}

