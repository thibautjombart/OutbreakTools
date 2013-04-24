\name{obkContacts-class}
\docType{class}
\alias{obkContacts}
\alias{obkContacts-class}
\alias{obkContactsOrNULL}
\alias{obkContactsOrNULL-class}

\alias{initialize,obkContacts-method}
\alias{show,obkContacts-method}
\alias{show.obkContacts}
\alias{print.obkContacts}
\alias{plot,obkContacts-method}
\alias{plot.obkContacts}

\alias{get.nindividuals,obkContacts-method}
\alias{get.individuals,obkContacts-method}

\alias{get.ncontacts,obkContacts-method}
\alias{get.contacts}
\alias{get.contacts,obkContacts-method}

\alias{get.ncontacts}
\alias{get.ncontacts,obkContacts-method}

\alias{network-class}
\alias{networkDynamic-class}
\alias{networkDynamicOrNetwork-class}
\alias{networkDynamicOrNetworkOrNULL-class}

% \alias{[,obkContacts-method}
% \alias{[,obkContacts,ANY,ANY-method}
%%%%
\title{Formal class "obkContacts"}
\description{
  The class \code{obkContacts} is a formal (S4) class for storing
  contact data between individuals.

  An \code{obkContacts} object can be constructed from individual
  identifiers (stored as \code{character} vectors), representing the
  'sender' and 'receiver' individuals, whether the contacts should be
  regarded as directed, with optional information about the timing of contacts.
}
\section{Objects from the class obkContacts}{
  \code{obkContacts} objects can be created by calls to \code{new("obkContacts",
    ...)}, where '...' can be the following arguments:
  
  \describe{
    \item{\code{contactsFrom}}{a vector of individual identifiers in
    \code{character} format representing the source of the contact.}
    \item{\code{contactsTo}}{a vector of individual identifiers in
      \code{character} format representing the sink of the contact.}
    \item{\code{directed}}{a Boolean for whether contacts should be
      regarded as directed (\code{TRUE}) or not (\code{FALSE}).}
    \item{\code{contactStart}}{an optional vector of times representing
      the start of a contact.}
    \item{\code{contactEnd}}{an optional vector of times representing the end of a
      contact.}
    \item{\code{duration}}{an optional vector of durations of a contact,
    as an alternative way of representing dynamic contacts than using \code{contactEnd}.}
  }
}
\section{Slots}{
  The following slots are the content of instances of the class
  \code{obkContacts}; note that in most cases, it is better to retrieve
  information via accessors (see below), rather than by accessing the
  slots manually.
  \describe{
    \item{\code{contacts}:}{an object of class \code{network} or \code{networkDynamic}.}
  }
}
\section{Methods}{
  Here is a list of methods available for \code{obkContacts} objects. Most of
  these methods are accessors, that is, functions which are used to
  retrieve the content of the object. Specific manpages can exist for
  accessors with more than one argument. These are indicated by a '*'
  symbol next to the method's name. This list also contains methods
  for conversion from \code{obkContacts} to other classes.
  \describe{
    \item{show}{\code{signature(x = "obkContacts")}: printing of the
      object.}
    \item{plot}{\code{signature(x = "obkContacts", y=NULL)}: plot the
    contact network; relies on \code{\link[network]{plot.network}}, to
    which arguments can be passed via \code{...}.}
    \item{get.nindividuals}{\code{signature(x = "obkContacts")}: returns the number of
      individuals in the sample.}
    \item{get.individuals}{\code{signature(x = "obkContacts")}: returns
    the identifiers of individuals in the sample.}
    \item{get.ncontacts}{\code{signature(x = "obkContacts")}: returns
    the number of contacts in the sample.}
    \item{get.contacts}{\code{signature(x = "obkContacts")}: returns the
    contacts (as \code{network} or \code{networkDynamic}).}
  }
}
\author{Simon Frost (\email{sdwfrost@gmail.com})}
\examples{

## THIS IS A TOY EXAMPLE ##
cf <- c("a", "b", "a", "c", "d")
ct <- c("b", "c", "c", "d", "b")
onset <- c(1, 2, 3, 4, 5)
terminus <- c(1.2, 4, 3.5, 4.1, 6)
oc.static <- new("obkContacts",cf,ct,FALSE) # static network
oc.dynamic <- new("obkContacts",cf,ct,FALSE,onset,terminus)
oc.static
oc.dynamic

plot(oc.static, main="Contact network")

}
\keyword{classes}
