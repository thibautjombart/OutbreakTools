\name{plot.ggphy}
\alias{plot.ggphy}
\title{Function to plot phylogenies of the class 'ggphy'}
\usage{
  plot.ggphy(ggphy, tip.labels = FALSE, tip.attribute = NULL,
    var.tip.labels = NULL, var.tip.colour = NULL)
}
\arguments{
  \item{ggphy}{An object of the class "ggphy"}

  \item{tip.labels}{Logical.Should tip labels be plotted?}

  \item{tip.attribute}{Dataframe with at least two columns.
  One column must contain the tip labels, the remainings
  are tip attributes}

  \item{var.tip.labels}{Character. The name of the column
  of tip.attribute that contains the tip labels.}

  \item{var.tip.colour}{Character. The name of the column
  of tip.attribute that contains the attribute to be
  colour-codded.}
}
\description{
  Function to plot phylogenies of the class 'ggphy'
}
\examples{
}
\author{
  Anton Camacho
}

