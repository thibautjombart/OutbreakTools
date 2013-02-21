% \encoding{UTF-8}
\name{get.phylo}
\alias{get.phylo}
\alias{get.phylo,obkData-method}
\alias{get.phylo.obkData}
\title{Obtain phylogenies from outbreak data}
\description{
  The function \code{get.phylo} is used to obtain phylogenies for each
  of the genes sequenced in an \linkS4class{obkData}
  object. Phylogenetic trees can optionally be plotted before being
  returned. Colors are used to distinguish samples, individuals, dates,
  or any other requested information. \code{get.phylo} is a wrapper for
  functions from the \code{ape} package: \code{\link[ape]{dist.dna}} for
  computing genetic distances, and specific methods for getting trees
  such as \code{\link[ape]{nj}}.
}
\usage{
get.phylo(x, \dots)

\S4method{get.phylo}{obkData}(x, locus=NULL, model = "N", pairwise.deletion = FALSE,
           method=nj, color.by=c("sample","individual","date"),
           palette=funky, plot=TRUE, ask=TRUE,\dots)
}
\arguments{
  \item{x}{an \linkS4class{obkData} object.}
  \item{locus}{a character or integer vector indicating the loci to be
    used; if NULL, all loci are used, producing one phylogeny each; see \code{\link{get.dna}}.}
  \item{model, pairwise.deletion}{arguments passed to \code{\link[ape]{dist.dna}}.}
  \item{method}{a function producing phylogenetic trees with the class
    \code{phylo} (ape package) from a distance matrix with class
    \code{dist}; examples include \code{\link[ape]{nj}},
    \code{\link[ape]{bionj}}, \code{\link[ape]{fastme.bal}},
    and \code{\link[ape]{fastme.ols}}.}
  \item{color.by}{a character string indicating the information to be
    used to color the tips of the tree; "sample", "individual", and
    "date" will be fetched from the appropriate fields in the
    \linkS4class{obkData} object; other fields present in
    \code{@samples} of the object can be used.}
  \item{palette}{a color palette to be used for the tip colors}
  \item{plot}{a logical indicating whether plots of the trees should be produced.}
  \item{ask}{a logical indicating whether user input must be waited for
    before drawing new plots.}
  \item{\dots}{further graphical arguments to be passed to \code{\link[ape]{plot.phylo}}.}
}
\value{
  \code{get.phylo} returns a list of items; the first item is a list of
  trees with \code{phylo} class, with one tree per requested
  locus; for each tree, tip colors are stored in a \code{$tip.col} slot;
  other slots of the main list are:
  \itemize{
    \item legend$col: colors to be used when plotting a legend
    \item legend$txt: annotations to be used when plotting a legend
  }
}
\author{Thibaut Jombart \email{t.jombart@imperial.ac.uk}}
% \examples{

% }
\keyword{classes}
