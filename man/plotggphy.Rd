\name{plotggphy}
\alias{plotggphy}
\title{Function to plot phylogenies using \code{ggplot2}}
\usage{
  plotggphy(x, which.tree = 1, ladderize = TRUE,
    show.tip.label = NULL, tip.label.size = 3,
    build.tip.attribute = TRUE, tip.color = NULL,
    tip.alpha = NULL, tip.shape = NULL, tip.size = NULL,
    branch.unit = NULL, tip.dates = NULL,
    guess.tip.dates.from.labels = FALSE,
    set.guess = list(prefix = "_", order = 1, from = "last"),
    axis.date.format = NULL, major.breaks = NULL,
    minor.breaks = NULL, color.palette = "Spectral",
    legend.position = "right")
}
\arguments{
  \item{x}{An \linkS4class{obkData} object.}

  \item{which.tree}{Numeric. Specify the order of the tree
  to be plotted. Currently the function cannot plot
  multiple tree.}

  \item{ladderize}{If \code{TRUE}, the phylogeny is
  ladderized}

  \item{show.tip.label}{Logical. If \code{TRUE}, the labels
  of the tip are shown; if \code{NULL} (default), labels are shown for
  tree sizes up to 50 tips.}

  \item{tip.label.size}{Numeric. Size of the tip labels.}

  \item{build.tip.attribute}{Logical. If \code{TRUE}, then
  a data frame \code{tip.attribute} is constructed by
  merging the data frames \code{individuals} and
  \code{samples}.}

  \item{tip.color}{Character. Can be either the name of a
  color (e.g. "Black") or the name of a column of
  \code{tip.attribute}. In the first case, all tips have
  the specified color. In the second case, tips are
  color-codded according to the specified attribute.}

  \item{tip.alpha}{Character (or Numeric). Can be either
  the value of transparency (between 0 and 1) or the name
  of a column of \code{tip.attribute}. In the first case,
  all tips have the specified transparency. In the second
  case, tips are transparency-codded according to the
  specified attribute.}

  \item{tip.shape}{Character (or Numeric). Can be either
  the value of a shape (e.g. 16 correpond to filled
  circles) or the name of a column of \code{tip.attribute}.
  In the first case, all tips have the specified shape. In
  the second case, tips are shape-codded according to the
  specified attribute.}

  \item{tip.size}{Character (or Numeric). Can be either the
  value of tip size or the name of a column of
  \code{tip.attribute}. In the first case, all tips have
  the specified size. In the second case, tips are
  size-codded according to the specified attribute.}

  \item{branch.unit}{Character. The unit of the branch can
  be either "year", "month", "day" or "subst". If a time
  unit is provided, together with use.tip.dates, then the
  x-axis of the phylogeny is plotted in date format using
  standard POSIX specification.}

  \item{tip.dates}{Character. If branch.unit is in unit of
  time, tip.dates indicates the name of the column of
  tip.attribute that contains the sampling dates of the
  tip. See also guess.tip.dates.from.labels.}

  \item{guess.tip.dates.from.labels}{Logical. If
  \code{TRUE} then tip.dates are guessed from the tip
  labels using the information provided by 'set.guess'.}

  \item{set.guess}{List. A list of three elements: prefix,
  order and from. For instance, if labels are formated like
  this: A/Shenzhen/40/2009_China_2009-06-09 then set.guess
  = list(prefix="_",order=3,from="first") or set.guess =
  list(prefix="_",order=1,from="last").}

  \item{axis.date.format}{Character. When x-axis is in date
  format, this argument allow to change the format of the
  tick labels. See strptime for more details.}

  \item{major.breaks}{Character. Major x-axis breaks (only
  when x is in date format). Ex: "weeks", "15days",
  "months", etc.}

  \item{minor.breaks}{Character. Minor x-axis breaks (only
  when x is in date format). Ex: "weeks", "15days",
  "months", etc.}

  \item{color.palette}{Character. The palette for tip
  colors. Only palettes from the package
  \code{\link{RColorBrewer}} are available. See
  \code{\link{brewer.pal}} documentation for more details.}

  \item{legend.position}{Character (or numeric). The
  position of legends. ("left", "right", "bottom", "top",
  or two-element numeric vector)}
}
\description{
  Function to plot phylogenies using \code{ggplot2}
}
\examples{
## load the dataset
data(FluH1N1pdm2009)
attach(FluH1N1pdm2009)

x <- new("obkData", individuals = individuals, dna = FluH1N1pdm2009$dna,
      dna.individualID = samples$individualID, dna.date = samples$date,
      trees = FluH1N1pdm2009$trees)

detach(FluH1N1pdm2009)


## have a look at the summary
summary(x)

## first simple tree
p <- plotggphy(x, ladderize = FALSE)

## build tip attribute and use sample dates to scale the x-axis as date time
p <- plotggphy(x, branch.unit = "year")

## change x breaks and labels
p <- plotggphy(x, branch.unit = "year", major.breaks = "month",
       axis.date.format = "\%b\%Y")

## color-code tip location
p <- plotggphy(x, branch.unit = "year", show.tip=FALSE, tip.color = "location")

## change tip size and transparency
p <- plotggphy(x, branch.unit = "year", tip.color = "location",
               tip.size = 3, tip.alpha = 0.75)


}
\author{
  Original versoin by Anton Camacho, modified by Thibaut Jombart.
}

