\name{plotIndividualTimeline}
\alias{plotIndividualTimeline}
\title{Plot a timeline of recorded data}
\description{
  This function plots a timeline of recorded data per individual, using
  an \linkS4class{obkData} object. Colors can be used to display
  meta-information on individuals, while different types of records are
  indicated using different symbols.
}
\usage{
 plotIndividualTimeline(x, what="", selection=NULL, ordering=NULL,
                        orderBy=NULL, colorBy=NULL, periods=NULL,
                        plotNames=length(selection)<50, \dots) }
\arguments{
  \item{x}{the main obkData object}

  \item{what}{a character string indicating which type of record should
  be displayed; partial matching is permitted; 'dna' should be used to
  include dates of DNA sequence collections; several values can be
  provided as a vector; if left by default (""), all available data are
  use.}

  \item{selection}{a vector of integers indicating the subset of
    individuals to plot}
  

  \item{ordering}{a vector of the same length as selection, which
    specifies the order of individuals on the plot. Overridden by
    \code{orderBy}.}
  
  \item{orderBy}{a character indicating the name of the column that
    should be used to order the individuals. Overrides \code{ordering}.}

  \item{colorBy}{a character indicating the name of the column by which
    individuals should be coloured}
  
  \item{periods}{an Nx2 matrix of strings, giving pairs of column names
    for periods to be plotted}
  
  \item{plotNames}{a logiacal indicating whether the individualIDs
    should be shown at the y-axis}

  \item{\dots}{further arguments passed to \code{geom_point}.}

}
\examples{
## simple example using ToyOutbreak
data(ToyOutbreak)
plotIndividualTimeline(ToyOutbreak)
plotIndividualTimeline(ToyOutbreak, what="DateInfected", colorBy="Sex")
plotIndividualTimeline(ToyOutbreak, what="DateInfected", colorBy="Age",
   orderBy="Sex")

plotIndividualTimeline(ToyOutbreak,selection=1:15,orderBy='lat')
plotIndividualTimeline(ToyOutbreak,selection=1:15,orderBy='lat',
   colorBy="Sex", size=4)


## example using HorseFlu
data(HorseFlu)

## plot all information, coloring by yard - messy!
plotIndividualTimeline(HorseFlu,colorBy='yardID')

## sort on yard, only DNA sequence collection
plotIndividualTimeline(HorseFlu, what="dna", orderBy='yardID',colorBy='yardID')

## just plot the first 30, vaccination dates
plotIndividualTimeline(HorseFlu, what=c("FirstVac","LastVac"),
   selection=1:30,orderBy='yardID',colorBy='yardID', size=3)


}
\author{
  Original version by Rolf Ypma,
  modified by Thibaut Jombart.
}

