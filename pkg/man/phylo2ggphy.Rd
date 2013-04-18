\name{phylo2ggphy}
\alias{phylo2ggphy}
\title{Function to convert phylogenies from the class 'phylo' to the class 'ggphy'}
\usage{
  phylo2ggphy(phylo, tip.dates = NULL, branch.unit = NULL,
    verbose = F)
}
\arguments{
  \item{phylo}{an object of the class "phylo"}

  \item{tip.dates}{a vector containing the sample dates of
  the tip in "Date" format, the dates must be ordered like
  the tips}

  \item{branch.unit}{the unit of the branch. Either "year",
  "month", "day" or "subst". If a time unit is provided,
  together with tip.dates, then the x-axis of the phylogeny
  will be in the Date format}
}
\description{
  Function to convert phylogenies from the class 'phylo' to
  the class 'ggphy'
}
\examples{
see misc/plot.ggphy.test.R
}
\author{
  Anton Camacho
}

