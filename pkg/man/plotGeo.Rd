\name{plotGeo}
\alias{plotGeo}
\title{Function to plot cases on a map}
\usage{
  plotGeo(data,location='location',isLonLat=FALSE,zoom='auto',source='google',colorBy=c(),center=c())

}
\arguments{
  \item{data}{the main obkData object}

  \item{location}{the name of the column holding strings representing geographical location, or two columns with lon/lat coordinates}

  \item{isLonLat}{specifies whether the data are already in lon/lat format}

  \item{zoom}{level of zooming. Higher number gives smaller scale. }

  \item{source}{source internet source to download maps}
  \item{colorBy}{attribute to color the nodes by}
  \item{center}{individualID of individual to put at the center of the map. If left empty, method will focus on center of all points}
}
\description{
   Function to plot a timeline for individuals involved in an outbreak in one plot
}
\examples{
## load the obkData object containing information about equine influenza outbreak
data(HorseFlu)
x <- HorseFlu

## plot the individuals on a map
plotGeo(x,location=c('lon','lat'),TRUE,zoom=8) # returns a warning message

## color by sex
plotGeo(x,location=c('lon','lat'),TRUE,zoom=8,colorBy='sex') # returns a warning message

## zoom in on the small cluster, by centering on individual '9'
plotGeo(x,location=c('lon','lat'),TRUE,colorBy='sex',zoom=12,center='9') # returns a warning message

## another example ##
## load obkData object containing data about a simulated outbreak
data(ToyOutbreak)

## plot the individuals on a map
plotGeo(ToyOutbreak,location=c('lon','lat'),isLonLat=TRUE,zoom=8)
plotGeo(ToyOutbreak,location=c('lon','lat'),TRUE,zoom=13,colorBy='Sex')

## color by age, zooming on the first case of the outbreak: indivudal 1
plotGeo(ToyOutbreak,location=c('lon','lat'),TRUE,zoom=15,colorBy='Age', center='1')



\dontrun{

## another example
data(rabies)
attach(rabies)
data <- new('obkData',individuals=data.frame(individualID=1:177,transmissions))
plotGeo(data,location=c('xinf','yinf'),isLonLat=TRUE)
plotGeo(data,location=c('xinf','yinf'),isLonLat=TRUE,zoom=7,colorBy='pr')

## not functional, but pretty
plotGeo(data,location=c('xinf','yinf'),isLonLat=TRUE,zoom=7,colorBy='source')

detach(rabies)
}
}
\author{
	Rolf Ypma
}
