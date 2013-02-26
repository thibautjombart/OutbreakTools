\name{obkClinicalEvent-class}
\docType{class}
\alias{obkClinicalEvent}
\alias{obkClinicalEventOrNULL-class}
\alias{obkClinicalEvent-class}
\alias{initialize,obkClinicalEvent-method}
\alias{show,obkClinicalEvent-method}

\alias{get.characteristic}
\alias{get.characteristic,obkClinicalEvent-method}

\alias{get.duration}
\alias{get.duration,obkClinicalEvent-method}

\alias{get.start.date}
\alias{get.start.date,obkClinicalEvent-method}

\alias{get.end.date}
\alias{get.end.date,obkClinicalEvent-method}

\alias{get.id,obkClinicalEvent-method}

\alias{get.type}
\alias{get.type,obkClinicalEvent-method}

%% model for method 'foo'
%\alias{foo}
%\alias{foo,obkClinicalEvent-method}
%%%%
\title{Formal class "obkClinicalEvent"}
\description{
  The class \code{obkClinicalEvent} is a formal (S4) class for storing
  clinical event information obtained during a disease outbreak.

  An \code{obkClinicalEvent} object can be constructed from ...
}
\section{Objects from the class obkClinicalEvent}{
  \code{obkClinicalEvent} objects can be created by calls to \code{new("obkClinicalEvent",
    ...)}, where '...' can be the following arguments:
  
  \describe{
    \item{\code{individual.ID}}{a ... .}
    \item{\code{type}}{a ... .}
    \item{\code{characteristic}}{a ... .}
    \item{\code{start.date}}{a ... .}
    \item{\code{end.date}}{a ... .}
    \item{\code{duration}}{a ... .}
    \item{\code{location}}{a ... .}
    \item{\code{format}}{a ... .}
  }
}
\section{Slots}{
  The following slots are the content of instances of the class
  \code{obkClinicalEvent}; note that in most cases, it is better to retrieve
  information via accessors (see below), rather than by accessing the
  slots manually.
    \describe{
    \item{\code{individual.ID}}{a ... .}
    \item{\code{type}}{a ... .}
    \item{\code{characteristic}}{a ... .}
    \item{\code{start.date}}{a ... .}
    \item{\code{end.date}}{a ... .}
    \item{\code{duration}}{a ... .}
    \item{\code{location}}{a ... .}
  }
}
\section{Methods}{
  Here is a list of methods available for \code{obkClinicalEvent} objects. Most of
  these methods are accessors, that is, functions which are used to
  retrieve the content of the object. Accessors with more than one
  argument are indicated by a '*' symbol next to the method's
  name. This list also contains methods for conversion from
  \code{obkClinicalEvent} to other classes.
  
  \describe{
    %    \item{show}{\code{signature(x = "obkClinicalEvent")}: .}
    \item{}{\code{signature(x = "obkClinicalEvent")}: .}
    \item{}{\code{signature(x = "obkClinicalEvent")}: .}
    \item{}{\code{signature(x = "obkClinicalEvent")}: .}
  
  }
}
\author{
  Put your name here.
}
\examples{

#### THIS EXAMPLE NEEDS TO BE FIXED ####
% ## THIS IS A TOY EXAMPLE ##

% ## test constructor / show
% new("obkClinicalEvent") # empty object


}
\keyword{classes}
