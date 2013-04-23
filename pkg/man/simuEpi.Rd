\name{simuEpi}
\alias{simuEpi}
\title{Simulate an epidemic following a SIR model}
\usage{
  simuEpi(N = 1000, D = 10, beta = 0.001, nu = 0.1, L = 1000, mu = 0.001, showPlots = FALSE,makePhyloTree=FALSE)
}
\arguments{
  \item{N}{Size of the population}

  \item{D}{Duration of simulation}

  \item{beta}{Rate of infection}

  \item{nu}{Rate of recovery}

  \item{L}{Length of genetic sequences}

  \item{mu}{Probability of mutation per base per transmission event}
  \item{showPlots}{Logical; whether or not to plot the SIR trajectory
    over time, the transmission tree, and the phylogenetic tree if it is
    created.}
  \item{makePhyloTree}{Logical; whether to create a
    neighbour-joining tree from the simulated sequences.}
}
\value{
  Simulated epidemic as an obkData object.
}
\description{
  Simulate an epidemic following a SIR model, together with a transmission
  tree and a set of sequences consistent with the trajectory of the epidemic. 
}
\author{
  Xavier Didelot and Caroline Colijn
}
