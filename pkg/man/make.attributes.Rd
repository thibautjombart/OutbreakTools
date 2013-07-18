% \encoding{UTF-8}
\name{make.tip.attributes}
\alias{make.tip.attributes}
\alias{make.sequence.attributes}
\title{Obtain meta data for items in \code{obkData} objects}
\description{
  The functions \code{make.sequence.attributes} and
  \code{make.tip.attributes} derive all available data relating to
  genetic sequences, based on the corresponding individuals and
  collection dates. The first returns information for all DNA sequences
  in the \linkS4class{obkData} object, while the second returns
  information for the tips of a given tree.
}
\usage{
make.sequence.attributes(x)

make.tip.attributes(x, which.tree=1)

}
\arguments{
  \item{x}{an \linkS4class{obkData} object.}
  \item{which.tree}{an integer indicating which tree to use.}
}
\value{
  A \code{data.frame} named after the sequences, with various meta-information.
}
\author{Thibaut Jombart \email{t.jombart@imperial.ac.uk}}
\examples{
data(ToyOutbreak)
head(make.sequence.attributes(ToyOutbreak))
}
\keyword{classes}
