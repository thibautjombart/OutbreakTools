\name{simuEpi}
\alias{simuEpi}
\title{Simulate an epidemic following a SIRS model}
\usage{
  simuEpi(N = 1000, D = 50, beta = 0.2, nu = 0.1, f = 0.5)
}
\arguments{
  \item{N}{Size of the population}

  \item{D}{Duration of simulation}

  \item{beta}{Rate of infection}

  \item{nu}{Rate of recovery}

  \item{f}{Rate of loss of immunity}
}
\value{
  simulated epidemic
}
\description{
  Simulate an epidemic following a SIRS model
}

