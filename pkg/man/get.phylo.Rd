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
\examples{

## Example 1: simulate outbreak ##

set.seed(3)
x <- simuEpi(N = 200, D = 20, beta = 0.002, nu = 0.1, mu = 0.002)

## get tree info ##
pal <- colorRampPalette(c("gold","red"))
treInfo <- get.phylo(x, color.by="date", palette=pal, plot=FALSE)
treInfo

## root tree to first case ##
tre <- root(treInfo[[1]], "1")

## plot tree, use color for dates ##
par(mar=c(1,1,4,1))
plot(tre, edge.width=2, type="fan", show.tip.label=FALSE)
title("NJ tree of a simulated outbreak")
mtext("each disk indicates 1 mutation from the root")

## add distance legend as bullseye ##
for(i in 1:11)
symbols(0,0, circles=i, inches=FALSE, add=TRUE, bg=transp("royalblue", .1), fg=NA)

## color tips by dates ##
tiplabels(col=tre$tip.col, pch=20)
tiplabels(col=transp(tre$tip.col), pch=20, cex=2)
legend("bottomleft", fill=treInfo$legend$col, legend=treInfo$legend$txt,title="Days after 1st sample")

## EXample 2: LOAD DATA ##

data(ToyOutbreak)

## get tree info for ToyOutbreak dataset ##
pal <- colorRampPalette(c("gold","red"))
treInfo <- get.phylo(ToyOutbreak, color.by="date", palette=pal, plot=TRUE, ask=TRUE)
treInfo


}
\keyword{classes}
