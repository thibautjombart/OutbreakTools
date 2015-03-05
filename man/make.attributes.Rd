% \encoding{UTF-8}
\name{make.tip.attributes}
\alias{make.tip.attributes}
\alias{make.individual.attributes}
\alias{make.sequence.attributes}
\title{Obtain meta data for items in \code{obkData} objects}
\description{
  Thes functions derive all available data relating to genetic
  sequences, tips of a tree, or individuals, based on matching
  individuals identifiers and collection dates. Note that for
  individuals, repeated measures will result in a 'wide' format for a
  data.frame, with possibly many missing entries resulting from uneven
  sampling.
}
\usage{
make.sequence.attributes(x)

make.tip.attributes(x, which.tree=1)

make.individual.attributes(x)

}
\arguments{
  \item{x}{an \linkS4class{obkData} object.}
  \item{which.tree}{an integer indicating which tree to use.}
}
\value{
  A \code{data.frame} named after the sequences, the tips, or the individuals, with various meta-information.
}
\author{Thibaut Jombart \email{t.jombart@imperial.ac.uk}}
\examples{
data(ToyOutbreak)
data(HorseFlu)

head(make.tip.attributes(ToyOutbreak))

head(make.sequence.attributes(HorseFlu))

## no repeated measures
head(make.individual.attributes(ToyOutbreak))

## note wide format when there are repeated measures
head(make.individual.attributes(HorseFlu))
}
\keyword{classes}
