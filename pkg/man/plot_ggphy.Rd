\name{plot_ggphy}
\alias{plot_ggphy}
\title{Function to plot phylogenies of the class 'ggphy'}
\usage{
  plot_ggphy(ggphy, tip_labels = F, tip_attribute = NULL,
    var_tip_labels = NULL, var_tip_colour = NULL)
}
\arguments{
  \item{ggphy}{An object of the class "ggphy"}

  \item{tip_labels}{Logical.Should tip labels be plotted?}

  \item{tip_attribute}{Dataframe with at least two columns.
  One column must contain the tip labels, the remainings
  are tip attributes}

  \item{var_tip_labels}{Character. The name of the column
  of tip_attribute that contains the tip labels.}

  \item{var_tip_colour}{Character. The name of the column
  of tip_attribute that contains the attribute to be
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

