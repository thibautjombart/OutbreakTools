\encoding{UTF-8}
\name{OutbreakTools-package}
\alias{OutbreakTools-package}
\alias{OutbreakTools}
\docType{package}
\title{The OutbreakTools package}
\description{
  \code{OutbreakTools} (previously known as 'epibase') is a package
  providing basic tools for the analysis of disease outbreaks. Its main
  features lie in handling possibly very different types of data within
  a coherent framework, represented by the class \linkS4class{obkData}
  (for "outbreak data"). This class allows to store and manipulate data
  on samples, individuals, records interventions, genetic sequences,
  phylogenetic trees and contact networks, most of this information
  being time-stamped and possibly geo-referenced.\cr

  For a more complete overview of the package, look at the tutorial
  vignette \code{OutbreakTools} accessible using:\cr
  \code{vignette("OutbreakTools", package="OutbreakTools")}\cr

  The main features of \code{OutbreakTools} include:
  \itemize{
    \item \linkS4class{obkData}: the main class of objects storing
    outbreak data; this formal class is provided with a number of
    accessors, documented in the same page.

    \item \linkS4class{obkSequences}: an auxiliary class for handling
    sequences from several genes.

    \item \linkS4class{obkContacts}: an auxiliary class for handling
    possibly dynamic contact networks between individuals.

    \item \code{\link{subset.obkData}}: (simply use \code{subset}) a
    method to subset the dataset using various criteria.

    \item \code{\link{make.phylo}}: create/plot phylogenies from the genes
    contained in an \linkS4class{obkData} object.

    \item \code{\link{read.annotated.nexus}}: read an annotated phylogeny
    into a phylo object.

    \item \code{\link{plotggMST}}: create/plot minimum spanning trees
    from the genes contained in an \linkS4class{obkData} object.

    \item \code{\link{dna2uniqSequences}}: derive unique sequences from
    a sequence alignment possibly containing identical genomes.

    \item \code{\link{plotIndividualTimeline}}: graphical representation
    of samples as times series.

    \item \code{\link{plotGeo}}: graphical representation of samples
    as geographic distributions.

    \item \code{\link{plotggphy}}: advanced graphical representation of
    phylogenetic trees using \code{ggplot2}.

    \item \code{\link{simuEpi}}: outbreak simulation using an SIR model
    and a simple model of sequence evolution.

    \item \code{datasets}: \code{\link{HorseFlu}},
    \code{\link{HorseFluRaw}}, \code{\link{ToyOutbreak}},
    \code{\link{ToyOutbreakRaw}}, \code{\link{FluH1N1pdm2009}}
  }

   To cite OutbreakTools, please use the reference given by
   \code{citation("OutbreakTools")}.
 }
\details{
  \tabular{ll}{
    Package: \tab OutbreakTools\cr
    Type: \tab Package\cr
    Version: \tab 0.1-0\cr
    Date: \tab 2014-01-10 \cr
    License: \tab GPL (>=2)
  }
}
\author{
  Maintainer: Thibaut Jombart <t.jombart@imperial.ac.uk>\cr
  Developers: David Aanensen, Marc Baguelin, Paul Birrell, Simon
  Cauchemez, Anton Camacho, Caroline Colijn, Anne Cori, Xavier Didelot,
  Ken Eames, Christophe Fraser, Simon Frost, Niel Hens, Joseph Hugues,
  Thibaut Jombart, Lulla Opatowski, Oliver Ratmann, Samuel Soubeyrand,
  Marc Suchard, Jacco Wallinga, Rolf Ypma
}

\keyword{manip}
\keyword{multivariate}
\seealso{
  OutbreakTools depends on or is related to several packages.
  The main non-base packages OutbreakTools relies on are:
  \itemize{
    \item ape: phylogenetic reconstruction
    \item ggplot2: advanced graphics
    \item network/networkDynamics: network handling
    \item sna: social network analysis
    \item ggmap: various map tools for ggplot2
  }

  Other useful packages include:
  \itemize{
    \item EpiEstim: reproduction number estimation
    \item adegenet: genetic data analysis
    \item outbreaker: outbreak reconstruction using pathogen sequences
    \item phangorn: maximum-likelihood phylogenetic reconstruction
    \item igraph: graph theory algorithms
  }

}

