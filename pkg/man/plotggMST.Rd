\name{plotggMST}
\alias{plotggMST}
\title{Function to plot a minimum spanning tree of the class 'obkData'}
\usage{
  plotggMST(x,individualID=NULL,locus=NULL)
}
\arguments{
  \item{x}{An object of the class "obkData"}
  \item{locus}{a character or integer indicating the loci to be used; if
    NULL, checks will be made to check that only one locus is in the
    object.}
  \item{individualID}{a character or integer to specify the individual
    identifier to draw the minimum spanning tree for.}
}

\description{
  Function to plot a minimum spanning tree for an individual
  using the mst function from ape and positioning the nodes according to
  the fruchtermanreingold in sna. The size of the nodes correspond to
  the number of copies for that particular sequence.
}

\examples{
\dontrun{
## load data
data(HorseFlu)
x <- HorseFlu

## plot minimum spanning tree for individual 42
plotggMST(x,individualID=42)

## another example data
data(ToyOutbreak)
x <- ToyOutbreak

## plot minimum spanning tree for gene1
plotggMST(x, locus="gene1")

}
}
\author{
  Joseph Hughes
}

