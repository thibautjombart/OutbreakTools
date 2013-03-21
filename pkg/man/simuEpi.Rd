\name{simuEpi}
\alias{simuEpi}
\title{Simulate an epidemic following a SIRS model}
\usage{
  simuEpi(N = 1000, D = 10, beta = 0.001, nu = 0.1, L = 1000, mu = 0.001, showPlots = FALSE)
}
\arguments{
  \item{N}{Size of the population}

  \item{D}{Duration of simulation}

  \item{beta}{Rate of infection}

  \item{nu}{Rate of recovery}

  \item{L}{Length of genetic sequences}

  \item{mu}{Probability of mutation per base per transmission event}
}
\value{
  Simulated epidemic as an obkData object
}
\description{
  Simulate an epidemic following a SIR model
}
\author{
  Xavier Didelot
}

