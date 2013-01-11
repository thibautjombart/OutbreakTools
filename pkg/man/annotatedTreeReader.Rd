\name{read.annotated.nexus}
\alias{read.annotated.nexus}
\title{Read annotated tree file in NEXUS format}
\usage{
  read.annotated.nexus(file)
}
\arguments{
  \item {file}{a file name specificed by either a variable 
   of mode character or a double-quoted string}  
}
\description{
  This function reads one or more annotated trees from a NEXUS formatted file.
}
\details{See read.nexus in the ape package for a specification of NEXUS formatted
tree files.  This function additionally extracts BEAST annotations for all
branches/nodes in the trees and returns these annotations as lists of lists in
the resulting "phylo" objects
}
\value{
  An object of class "phylo" with an additional slot called annotations.  
  This slot is a list indexed by the nodes.
\examples{
see misc/annotated_example.R
}
\author{
  Marc Suchard
}

