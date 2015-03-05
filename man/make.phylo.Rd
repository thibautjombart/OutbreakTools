% \encoding{UTF-8}
\name{make.phylo}
\alias{make.phylo}
\alias{make.phylo,obkData-method}
\alias{make.phylo.obkData}
\title{Obtain phylogenies from outbreak data}
\description{
  The function \code{make.phylo} is used to obtain phylogenies for each
  of the genes sequenced in an \linkS4class{obkData}
  object. Phylogenetic trees can optionally be plotted before being
  returned. Colors are used to distinguish samples, individuals, dates,
  or any other requested information. \code{make.phylo} is a wrapper for
  functions from the \code{ape} package: \code{\link[ape]{dist.dna}} for
  computing genetic distances, and specific methods for getting trees
  such as \code{\link[ape]{nj}}.
}
\usage{
make.phylo(x, \dots)

\S4method{make.phylo}{obkData}(x, locus=NULL,
           result=c("obkData","multiPhylo"), model = "N",
           pairwise.deletion = FALSE, method=nj,
           plot=FALSE, ask=TRUE,\dots)
}
\arguments{
  \item{x}{an \linkS4class{obkData} object.}
  \item{result}{a character string indicating the type of result to be
    returned; if \code{obkData}, then the phylogenies are stored
    in \code{x@trees}, possibly removing previous phylogenies; if
    \code{multiPhylo}, the result is a list of \code{phylo}
    objects with the class \code{multiPhylo}.}
  \item{locus}{a character or integer vector indicating the loci to be
    used; if NULL, all loci are used, producing one phylogeny each; see
    \code{\link{get.dna}}.}
  \item{model, pairwise.deletion}{arguments passed to \code{\link[ape]{dist.dna}}.}
  \item{method}{a function producing phylogenetic trees with the class
    \code{phylo} (ape package) from a distance matrix with class
    \code{dist}; examples include \code{\link[ape]{nj}},
    \code{\link[ape]{bionj}}, \code{\link[ape]{fastme.bal}},
    and \code{\link[ape]{fastme.ols}}.}
  % \item{color.by}{a character string indicating the information to be
  %   used to color the tips of the tree; "sample", "individual", and
  %   "date" will be fetched from the appropriate fields in the
  %   \linkS4class{obkData} object; other fields present in
  %   \code{@samples} of the object can be used.}
  % \item{palette}{a color palette to be used for the tip colors}
  \item{plot}{a logical indicating whether plots of the trees should be produced.}
  \item{ask}{a logical indicating whether user input must be waited for
    before drawing new plots.}
  \item{\dots}{further graphical arguments to be passed to \code{\link[ape]{plot.phylo}}.}
}
\value{
  A \code{obkData} with new phylogenies, or a list with class
  \code{multiPhylo}.
}
\seealso{
  \code{\link{read.annotated.tree}} and \code{\link{read.annotated.nexus}} to 
  read annotated phylogenies into phylo objects.
}
\author{Thibaut Jombart \email{t.jombart@imperial.ac.uk}}
\examples{

## Example using simulate outbreak ##

set.seed(3)
x <- simuEpi(N = 200, D = 20, beta = 0.002, nu = 0.1, mu = 0.002)$x
get.trees(x) # no tree here

## GET TREE AND DISPLAY IT
x <- make.phylo(x, plot=TRUE)
get.trees(x) # newly created trees


## DISPLAY TREE USING PLOTGGPHY

## root tree to first case ##
if(require(ape)){
tre <- root(get.trees(x)[[1]],1)
}

## plot tree, use color for dates ##
par(mar=c(1,1,4,1))
plot(tre, edge.width=2, type="fan", show.tip=FALSE)
title("NJ tree of a simulated outbreak")
mtext("each disk indicates 1 mutation from the root")

}
\keyword{classes}
