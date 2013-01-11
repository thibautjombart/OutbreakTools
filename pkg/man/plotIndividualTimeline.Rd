\name{plot.individualTimeline}
\alias{plot.individualTimeline}
\title{Function to plot a timeline for individuals involved in an outbreak in one plot}
\usage{
  plot.individualTimeline(data,selection=1:dim(data)[1],ordering=1:length(selection),orderBy=NULL,colorBy=NULL,events=NULL,clinicalEvents=NULL,periods=NULL)

}
\arguments{
  \item{data}{the main dataframe holding data on the individuals}

  \item{selection}{a vector of integers indicating the subset of the dataframe to use. }

  \item{ordering}{a vector of the same length as selection, which specifies the order of individuals on the plot. Overridden by orderBy}

  \item{orderBy}{string giving the name of the column that should be used to order the individuals. Overrides ordering}

  \item{colorBy}{string giving the name of the column by which individuals should be coloured}
  \item{events}{vector of strings giving the names of the columns that hold data values, to be marked on the time line}
  \item{clinicalEvents}{vector of integers giving the indices of the list clinicalData that should be plotted}
  \item{periods}{n X 2 matrix of strings giving the pairs of names for Date columns that should be plotted as periods on the time line}
}
\description{
   Function to plot a timeline for individuals involved in an outbreak in one plot
}
\examples{
plot.individualTimeline(individualData,orderBy="exposure.code",selection=1:20,clinicalEvents = 1:4,colorBy="exposure.code")

}
\author{
	Rolf Ypma
}

