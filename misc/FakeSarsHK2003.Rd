\name{FakeSarsHK2003}
\docType{data}
\alias{FakeSarsHK2003}

\title{Fake dataset similar to the SARS outbreak in Hong Kong in 2003}
\description{
  This data set is a fake data set that was designed to reproduce interesting features of the SARS 
  (severe acute respiratory syndrom) outbreak in Hong Kong in 2003. 
  It has a similar size and dynamics, as well as realistic breakdown of cases 
  according to their source of infection. 


  The dataset contains a list (called FakeSarsHK2003) of two objects.
  
  \code{Patients} is a \code{data.frame} containing individual identifiers and an exposure code that can take 
  the following values: 
  \itemize{
    \item{1 if individual was infected in Prince of Wales hospital 
      (where a large spreading event (SSE) occurred),}
    \item{2 if individual was infected in Amoy Gardens residence (where another SSE occurred),}
    \item{3 if individual was infected in another hospital,}
    \item{4 if source of infection is unknown.}
  }
  
  \code{Clinical} is a list of the following dataframes: 
  \itemize{
    \item{\code{hospitalisation} contains individuals ID and the date of admission 
      and discharge from hospital,}
    \item{\code{death} contains individuals ID and their date of death,}
    \item{\code{exposure} contains individuals ID and dates of start and end of exposure,
      defined for individuals who are known to have been in contact with known infected individuals 
      during this period of time,}
    \item{\code{symptoms} contains individuals ID and their dates of symptoms onset.}
  }
 
}
\keyword{dataset}
\author{
  Anne Cori
}
