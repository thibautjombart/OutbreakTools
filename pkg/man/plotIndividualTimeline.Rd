\name{plotIndividualTimeline}
\alias{plotIndividualTimeline}
\title{Function to plot a timeline for individuals involved in an outbreak in one plot}
\usage{
 plotIndividualTimeline(x, selection=1:dim(get.data(x,'individuals'))[1],
                        ordering=1:length(selection), orderBy=NULL, colorBy=NULL,
                        events=NULL, clinicalEvents=NULL, periods=NULL,
                        plotSamples=TRUE, plotNames=length(selection)<50)
}
\arguments{
  \item{x}{the main obkData object}

  \item{selection}{a vector of integers indicating the subset of individuals to plot }

  \item{ordering}{a vector of the same length as selection, which specifies the order of individuals on the plot. Overridden by orderBy}

  \item{orderBy}{string giving the name of the column that should be used to order the individuals. Overrides ordering}

  \item{colorBy}{string giving the name of the column by which
    individuals should be coloured}
  
  \item{events}{vector of strings giving the names of the columns that
    hold date values in 'individuals', to be marked on the time line}
  
  \item{clinicalEvents}{vector of strings giving the names of the
  columns that hold date values in 'clinical', to be marked on the time
  line}

  \item{periods}{an Nx2 matrix of strings, giving pairs of column names
    for periods to be plotted}
  
  \item{plotSamples}{should the samples (at sampling time) be plotted?}
  
  \item{plotNames}{ should the individualIDs be shown at the y-axis?}
}
\description{
   Function to plot a timeline for individuals involved in an outbreak in one plot
}
\examples{
## THIS EXAMPLE NEEDS TO BE FIXED ##

## load equine influenza data and convert it to obkData
data(HorseFlu)
data <- new("obkData",individuals=HorseFlu$individuals,samples=HorseFlu$samples,
clinical=HorseFlu$clinics, date.format="\%d/\%m/\%Y")

## plot the horses over time, coloring by yard
plotIndividualTimeline(data,colorBy='yardID')

## also sort on yard
plotIndividualTimeline(data,orderBy='yardID',colorBy='yardID')

## just plot the first 15
plotIndividualTimeline(data,selection=1:15,orderBy='yardID',colorBy='yardID')

## do a simple plot for some influenza data
data(FakeInfluenza)
attach(FakeInfluenza)
data <- new("obkData", individuals=Patients, samples=Samples, clinical=Clinical, date.format="\%d/\%m/\%Y")
plotIndividualTimeline(data,colorBy='gender')
detach(FakeInfluenza)
}
\author{
	Rolf Ypma
}

