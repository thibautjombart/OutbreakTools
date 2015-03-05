\name{simuEpi}
\alias{simuEpi}
\title{Simulate an epidemic following a SIR model}
\description{
  Simulate an epidemic following a SIR model, together with a
  transmission tree and a set of sequences consistent with the
  trajectory of the epidemic.
}
\usage{
  simuEpi(N = 1000, D = 10, beta = 0.001, nu = 0.1, L = 1000, mu =
          0.001, plot=TRUE, makePhyloTree=FALSE)
}
\arguments{
  \item{N}{Size of the population}

  \item{D}{Duration of simulation}

  \item{beta}{Rate of infection}

  \item{nu}{Rate of recovery}

  \item{L}{Length of genetic sequences}

  \item{mu}{Probability of mutation per base per transmission event}
  \item{plot}{logical indicating whether or not to plot the SIR trajectory
    over time and save it in the output.}

  \item{makePhyloTree}{Logical; whether to create a
    neighbour-joining tree from the simulated sequences.}
}
\value{
  A list containing the SIR dynamics (\code{$dynamics}), an
    \code{obkData} of the outbreak (\code{$x}), and an optional \code{ggplot}
    graphic (\code{$plot}).
}

\examples{
\dontrun{
## Simulate an outbreak of 200 individuals over 20 time steps ##
set.seed(3)

x <- simuEpi(N = 200, D = 20, beta = 0.002, nu = 0.1, mu = 0.002)

## x is a list:
class(x)
names(x)

## x$dynamics contains demographic info
x$dynamics

## x$plot is a ggplot object
class(x$plot)
x$plot

## x$x is the obkData object
summary(x$x)
plotIndividualTimeline(x$x, colorBy="DateInfected")


## Same, with a phylogenetic tree
x <- simuEpi(N = 200, D = 20, beta = 0.002, nu = 0.1, mu = 0.002,
             plot=TRUE, makePhyloTree=TRUE)

plotggphy(x$x, ladderize=TRUE, show.tip=TRUE,
      branch.unit="year", tip.label.size=4)
}
}

\author{
  Original version by Xavier Didelot and Caroline Colijn.
  Graphics modification by Thibaut Jombart.
}
