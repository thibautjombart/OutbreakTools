\encoding{UTF-8}
\name{read annotated trees}
\alias{read.annotated.tree}
\alias{read.annotated.nexus}
\title{Read annotated tree files in Newick or NEXUS format}
\description{
  These functions read annotated trees from files in Newick or NEXUS
  formats. Except for the annotations, these functions mimic ape's
  functions \code{\link[ape]{read.tree}} and \code{\link[ape]{read.nexus}}.

  Annotations are ordered by edges, i.e. matching the \code{edge.length}
  slot of a \code{phylo} object.
}
\usage{
  read.annotated.tree(file="", text = NULL, tree.names = NULL, skip = 0,
                      comment.char = "#", keep.multi = FALSE, \dots)
  read.annotated.nexus(file, tree.names = NULL)

}
\arguments{
  \item{file}{a file name specified by either a variable of mode character,
          or a double-quoted string; if ‘file = ""’ (the default) then
          the tree is input on the keyboard, the entry being terminated
          with a blank line}
  \item{text}{alternatively, the name of a variable of mode character which
          contains the tree(s) in parenthetic format. By default, this
          is ignored (set to ‘NULL’, meaning that the tree is read in a
          file); if ‘text’ is not ‘NULL’, then the argument ‘file’ is
          ignored.}
  \item{tree.names}{if there are several trees to be read, a vector of mode
          character that gives names to the individual trees; if ‘NULL’
          (the default), the trees are named ‘"tree1"’, ‘"tree2"’, ...}
  \item{skip}{the number of lines of the input file to skip before
          beginning to read data (this is passed directly to‘ scan()’).}
  \item{comment.char}{a single character, the remaining of the line after this
          character is ignored (this is passed directly to ‘scan()’).}
  \item{keep.multi}{if ‘TRUE’ and ‘tree.names = NULL’ then single trees are
          returned in ‘"multiPhylo"’ format, with any name that is
          present (see details). Default is ‘FALSE’.}
  \item{\dots}{further arguments to be passed to ‘scan()’.}
}
\details{
  See \code{\link[ape]{read.nexus}} in the ape package for a specification of NEXUS formatted
  tree files.  This function additionally extracts BEAST annotations for all
  branches/nodes in the trees and returns these annotations as lists of lists in
  the resulting "phylo" objects
}
\value{
  An object of class "phylo" with an additional slot called annotations.
  This slot is a list indexed by the nodes.
}
%\examples{
%    see misc/annotated_example.R
%}
\author{
  Marc Suchard
}
\examples{
\dontrun{
## read annotated tree from Nexus file
## (note: do not use 'system file' for your own file!)
tre <- read.annotated.nexus(system.file("files/BEAST-expl.nex", package="OutbreakTools"))

## ladderize the tree
tre <- ladderize(tre)

## this tree has annotations
##
names(tre)
class(tre$annotations)
length(tre$annotations)

## for each edge (each edge is identified by a terminal node), we have:
tre$annotations[[1]]
names(tre$annotations[[1]])

## extract rates from annotations
rates <- unlist(sapply(tre$annotations, function(e) e$rate_median))

## plot tree, show median rates as colors
plot(tre, show.tip=FALSE, edge.col=num2col(rates, col.pal=seasun))

}
}

