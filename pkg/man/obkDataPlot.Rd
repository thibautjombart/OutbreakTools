\encoding{UTF-8}
\name{Plot obkData}
\alias{plot.obkData}
\alias{plot,obkData-method}
\title{Plot outbreak data}
\description{
  The plot method of \linkS4class{obkData} objects includes various
  options:
  \itemize{
    \item \code{timeline}: plots requested information as time series;
    uses \code{\link{plotIndividualTimeline}}.

    \item \code{geo}: plots requested information as geographic
    distribution, on a map; uses \code{\link{plotGeo}}.

    \item \code{mst}: minimum-spanning tree based on genetic distances;
    uses \code{\link{plotggMST}}.

    \item \code{phylo}: plot phylogenetic trees stored in the object;
    uses \code{\link{plotggphy}}.

   \item \code{contacts}: plot contacts between individuals stored in the object;
    uses \code{\link{plot.obkContacts}}.
}
}
\usage{
\S4method{plot}{obkData}(x, y=c("timeline","geo","mst","phylo","contacts"), \dots)
}
\arguments{
  \item{x}{a \linkS4class{obkData} object}
  \item{y}{a character indicating the type of plot to be generated; can
    be "timeline" (default), "geo", "mst", "phylo", or "contacts".}
  \item{\dots}{further arguments to be passed to the plotting function,
    depending on the type of plot selected (see description above).}
}
\seealso{
  \code{\link{plotIndividualTimeline}}, \code{\link{plotGeo}},
  \code{\link{plotggMST}}, \code{\link{plotggphy}}, \code{\link{plot.obkContacts}}.
}
\author{
  Rolf Ypma  \email{Rolf.Ypma@rivm.nl}, 
  Thibaut Jombart \email{t.jombart@imperial.ac.uk}
}
\examples{
data(HorseFlu)
head(HorseFlu)

## default plot: time line
plot(HorseFlu,orderBy='yardID',colorBy='yardID')
plot(HorseFlu,orderBy='yardID',colorBy='yardID',
     selection=1:30) # only the first 30

## plot contacts
data(ToyOutbreak)
plot(ToyOutbreak, "contacts")

\dontrun{
## plot geographic distribution of individuals
plot(HorseFlu, "geo", location=c('lon','lat'), colorBy='sex',
     zoom=12,center='9')

## plot minimum spanning tree for first 10 individuals
get.nindividuals(HorseFlu)
plot(subset(HorseFlu, individuals=1:10), "mst")
}
}
\keyword{multivariate}