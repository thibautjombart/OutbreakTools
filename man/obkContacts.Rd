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
\alias{plot,obkContacts,ANY-method}
\alias{plot.obkContacts}

\alias{get.ncontacts,obkContacts-method}
\alias{get.ncontacts.obkContacts}

\alias{get.contacts}
\alias{get.contacts,obkContacts-method}
\alias{get.contacts.obkContacts}

\alias{get.ncontacts}
\alias{get.ncontacts,obkContacts-method}
\alias{get.ncontacts.obkContacts}

\alias{network-class}
\alias{networkDynamic-class}
\alias{networkDynamicOrNetwork-class}
\alias{networkDynamicOrNetworkOrNULL-class}

\alias{as.matrix,obkContacts-method}
\alias{as.matrix.obkContacts}

\alias{as.data.frame,obkContacts-method}
\alias{as.data.frame.obkContacts}

% \alias{[,obkContacts-method}
% \alias{[,obkContacts,ANY,ANY-method}
%%%%
\title{Formal class "obkContacts"}
\description{
  The class \code{obkContacts} is a formal (S4) class for storing
  contact data between individuals. Contacts can be static, or change in
  time (dynamic) at specified dates. Contacts are handled as
  \code{network} and \code{networkDynamic} objects, for static and
  dynamic contacts, respectively. See the corresponding packages for
  more information on this data structure.

  An \code{obkContacts} object can be constructed from individual
  identifiers (stored as \code{character} vectors), representing the
  'sender' and 'receiver' individuals, whether the contacts should be
  regarded as directed, with optional information about the timing of
  contacts.
}
\section{Objects from the class obkContacts}{
  \code{obkContacts} objects can be created by calls to
  \code{new("obkContacts", ...)}, where '...' can be the following
  arguments:

  \describe{
    \item{\code{from}}{a vector of individual identifiers in
      \code{character} format representing the source of the contact.}
    
    \item{\code{to}}{a vector of individual identifiers in
      \code{character} format representing the sink of the contact.}
    
    \item{\code{directed}}{a Boolean for whether contacts should be
      regarded as directed (\code{TRUE}) or not (\code{FALSE}).}
    
    \item{\code{start}}{an optional vector of times representing the
      start of a contact; can be dates or numeric values.}
    
    \item{\code{end}}{an optional vector of times representing the end
      of a contact; can be dates or numeric values..}
    
    \item{\code{duration}}{an optional vector of durations of a contact,
      as an alternative way of representing dynamic contacts than using
      \code{end}; if numeric values are used together with dates in
      'start' and 'end', these will be interpreted as numbers of days.}

  }
}
\section{Slots}{
  The following slots are the content of instances of the class
  \code{obkContacts}; note that in most cases, it is better to retrieve
  information via accessors (see below), rather than by accessing the
  slots manually.
  \describe{
    \item{\code{contacts}:}{an object of class \code{network} or \code{networkDynamic}.}
    \item{\code{origin}:}{an object of class \code{Date} indicating the
      origin of the contacts (i.e., this corresponds to "day 0".}
  }
}
\section{Methods}{
  Here is a list of methods available for \code{obkContacts}
  objects. Most of these methods are accessors, that is, functions which
  are used to retrieve the content of the object. Specific manpages can
  exist for accessors with more than one argument. These are indicated
  by a '*' symbol next to the method's name. This list also contains
  methods for conversion from \code{obkContacts} to other classes.

  \describe{
    \item{show}{\code{signature(x = "obkContacts")}: printing of the
      object.}
    
    \item{plot}{\code{signature(x = "obkContacts", y=NULL)}: plot the
      contact network; relies on \code{\link[network]{plot.network}}, to
      which arguments can be passed via \code{...}.}

    \item{get.ncontacts}{\code{signature(x = "obkContacts")}: returns
      the number of contacts; for dynamic contact
      networks, starting dates can be specified by the argument
      \code{from}, and end date by the argument \code{to}.}
    
    \item{get.contacts}{\code{signature(x = "obkContacts")}: returns
      the contacts (as \code{network} or \code{networkDynamic}); for
      dynamic contact networks, starting dates can be specified by the
      argument \code{from}, and end date by the argument \code{to}.}

    \item{get.nindividuals*}{\code{signature(x = "obkContacts")}: returns the number of
      individuals in the contact data (see manpage
      of \code{\link{get.individuals}} for details)..}
    
    \item{get.individuals*}{\code{signature(x = "obkContacts")}: returns
      the identifiers of individuals in the contact data(see manpage
      of \code{\link{get.individuals}} for details)..}
    
    \item{get.ndates*}{\code{signature(x = "obkContacts")}: returns the number of
      dates at which contact structure changes (see manpage of
      \code{\link{get.dates}} for details).}
    
    \item{get.dates*}{\code{signature(x = "obkContacts")}: returns the
      dates at which contact structure changes (see manpage of
      \code{\link{get.dates}} for details).}
    
    \item{as.matrix}{\code{signature(x = "obkContacts",
	matrix.type=c("adjacency","incidence","edgelist"))}: extract an
      adjacency, incidence, or edge matrix from the object.}
    
    \item{as.data.frame}{\code{signature(x = "obkContacts", ...)}: for
      static contacts, returns an edge list; for dynamic contacts, returns
      and edge list with information on the timing of contacts.}
  }
}
\usage{
get.contacts(x, \dots)
get.ncontacts(x, \dots)
\S4method{get.ncontacts}{obkContacts}(x, from=NULL, to=NULL, \dots)
\S4method{get.contacts}{obkContacts}(x, from=NULL, to=NULL, \dots)

\S4method{show}{obkContacts}(object)

\S4method{plot}{obkContacts}(x, y=NULL, labels=get.individuals(x),
\dots)

\S4method{as.matrix}{obkContacts}(x,
         matrix.type=c("adjacency","incidence","edgelist"),
         use.labels=TRUE, \dots)

\S4method{as.data.frame}{obkContacts}(x, row.names=NULL, optional=FALSE,
         use.labels=TRUE, \dots)

}
\arguments{
  \item{x,object}{an obkContacts object.}
  
  \item{y}{used for compatibility with the generic definition of
    \code{plot}; unused.}
  
  \item{labels}{a vector of characters giving annotation for the
    vertices.}
  
  \item{matrix.type}{a character indicating which type of matrix to
    extract(adjacency, incidence, or edge matrix).}
  
  \item{from,to}{dates indicating the time span to consider for active
    contacts (for dynamic networks only).}
  
 \item{\dots}{arguments passed to other methods; currently not used,
   except for \code{plot}.}
 
 \item{row.names,optional}{unused arguments, there for compatibility
   with the generic \code{as.data.frame}.}
 
 \item{use.labels}{a logical indicating whether labels should be used to
   indicate individuals, as opposed to their index in the network.}
 
}
\author{
  Original class: Simon Frost (\email{sdwfrost@gmail.com})\cr
  Plot, various accessors, conversion methods: Thibaut Jombart (\email{tjombart@imperial.ac.uk})
}
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



## PLOTTING A DYNAMIC CONTACT NETWORK ##
## LOAD DATA
data(ToyOutbreak) # ToyOutbreak includes a dynamic contact network

## PLOT THE STATIC NETWORK
plot(ToyOutbreak@contacts)

## PLOT THE DYNAMIC NETWORK OVER THE FIRST 4 DAYS OF THE OUTBREAK
par(mfrow=c(2,2))

plot(ToyOutbreak@contacts)

plot(get.contacts(ToyOutbreak, from=0, to=2), displaylabels=TRUE,
     main="Contact network - days 1-2")

plot(get.contacts(ToyOutbreak, from=2.9, to=3.1), displaylabels=TRUE,
     main="Contact network - day 3")

plot(get.contacts(ToyOutbreak, from=3.9, to=4.1), displaylabels=TRUE,
     main="Contact network - day 4")

par(mfrow=c(1,1))

# extract matrices of adjacency, incidence, or edge list
as.matrix(ToyOutbreak@contacts)
as.matrix(ToyOutbreak@contacts, "incidence")
as.matrix(ToyOutbreak@contacts, "edgelist")

as.data.frame(ToyOutbreak@contacts)

}
\keyword{classes}
