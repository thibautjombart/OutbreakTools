\name{plotGeo}
\alias{plotGeo}
\title{Function to plot cases on a map}
\usage{
  plotGeo(x, location=NULL, zoom='auto', source='google',
          colorBy=NULL, shapeBy=NULL, center=NULL, \dots)

}
\arguments{
  \item{x}{the main \linkS4class{obkData} object}

  \item{location}{the name of the columns containing location data; if
    a vector of length 2 is provided, these are expected to be
    longitudes and latitudes.}

 \item{zoom}{a numeric indicating the level of zooming; higher number
   gives smaller scale (higher zoom).}

  \item{source}{a character string indicating the internet source from
    which to download maps.}
  
  \item{colorBy}{the name of the attribute to use for coloring the symbols.}

  \item{shapeBy}{the name of the attribute to use for shaping the symbols.}
  
  \item{center}{individualID of individual to put at the center of the
    map. If left empty, method will focus on the center of all points}

  \item{\dots}{further arguments passed to \code{geom_point}.}
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
plotGeo(x,location=c('lon','lat'),zoom=8)

## color by sex
plotGeo(x,location=c('lon','lat'),zoom=8,colorBy='sex')

## zoom in on the small cluster, by centering on individual '9'
plotGeo(x,location=c('lon','lat'),colorBy="sex",zoom=14,center='9',size=4,
  alpha=(.7))

plotGeo(x,location=c('lon','lat'),colorBy="yardID", shapeBy="sex", zoom=14,center='9',size=4,
  alpha=(.7))

## another example ##
## load obkData object containing data about a simulated outbreak
data(ToyOutbreak)

## plot the individuals on a map
plotGeo(ToyOutbreak,location=c('lon','lat'), zoom=8)
plotGeo(ToyOutbreak,location=c('lon','lat'), zoom=13, colorBy='Sex', size=3)

## color by age, zooming on the first case of the outbreak: indivudal 1
plotGeo(ToyOutbreak,location=c('lon','lat'), zoom=15,
        colorBy='Age', center='1', size=5)
}

}
\author{
  Original version by Rolf Ypma.
  Tweaks by Thibaut Jombart.
}
