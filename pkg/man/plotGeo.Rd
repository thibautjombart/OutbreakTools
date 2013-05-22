\name{plotGeo}
\alias{plotGeo}
\title{Function to plot cases on a map}
\usage{
  plotGeo(data,location='location',isLonLat=FALSE,zoom='auto',
          source='google',colorBy=c(),center=c() )

}
\arguments{
  \item{data}{the main obkData object}

  \item{location}{the name of the column holding strings representing
    geographical location, or two columns with lon/lat coordinates}

  \item{isLonLat}{specifies whether the data are already in lon/lat format}

  \item{zoom}{level of zooming. Higher number gives smaller scale. }

  \item{source}{source internet source to download maps}
  \item{colorBy}{attribute to color the nodes by}
  \item{center}{individualID of individual to put at the center of the
    map. If left empty, method will focus on center of all points}
}
\description{
   Function to plot the geographic distribution of data in an
   \linkS4class{obkData}.
}
\examples{
\dontrun{
## load the obkData of equine influenza outbreak
data(HorseFlu)
x <- HorseFlu

## plot the individuals on a map
plotGeo(x,location=c('lon','lat'),TRUE,zoom=8)

## color by sex
plotGeo(x,location=c('lon','lat'),TRUE,zoom=8,colorBy='sex')

## zoom in on the small cluster, by centering on individual '9'
plotGeo(x,location=c('lon','lat'),TRUE,colorBy='sex',zoom=12,center='9')

## another example ##
## load obkData object containing data about a simulated outbreak
data(ToyOutbreak)

## plot the individuals on a map
plotGeo(ToyOutbreak,location=c('lon','lat'),isLonLat=TRUE,zoom=8)
plotGeo(ToyOutbreak,location=c('lon','lat'),TRUE,zoom=13,colorBy='Sex')

## color by age, zooming on the first case of the outbreak: indivudal 1
plotGeo(ToyOutbreak,location=c('lon','lat'),TRUE, zoom=15,
        colorBy='Age', center='1')
}

}
\author{
	Rolf Ypma
}
