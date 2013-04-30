
############################
####  CLASS DEFINITION  ####
############################

## CLASS DESCRIPTION:
## - instance of obkContacts store contacts between individuals
## - employs networkDynamic

setClass("obkContacts",representation(contacts="networkDynamicOrNetworkOrNULL"),prototype(contacts=NULL))

setClassUnion("obkContactsOrNULL", c("obkContacts", "NULL"))




######################
####  CONSTRUCTOR ####
######################

## INPUT DESCRIPTION:
## contactFrom: a vector of characters indicating IDs of "sender" individuals
## contactTo: a vector of characters indicating IDs of "receiver" individuals
## directed: should we consider the network as directed
## contactStart: if present, a vector of dates of beginning of contact
## contactEnd: if present, a vector of dates of end of contact
## duration: another way to specify contactEnd, as duration of contact
setMethod("initialize", "obkContacts", function(.Object, contactFrom=NULL, contactTo=NULL, directed=FALSE, contactStart=NULL, contactEnd=NULL,duration=NULL) {

    ## RETRIEVE PROTOTYPED OBJECT ##
    x <- .Object

    ## escape if the minimum information is not provided ##
    if(is.null(contactFrom)|is.null(contactTo)) return(x)

    ## PROCESS ARGUMENTS
    if(is.list(contactFrom)) contactFrom <- unlist(contactFrom)

    ## fill slots
    contactFrom <- as.character(contactFrom)
    contactTo <- as.character(contactTo)
    uniqueIDs <- unique(c(contactFrom,contactTo))
    numIDs <- length(uniqueIDs)
    numedges <- length(contactFrom)
    y <- network.initialize(numIDs,directed=directed,multiple=TRUE)
    network.vertex.names(y) <- uniqueIDs
    ## static network
    if(is.null(contactStart)){
      for(i in 1:numedges){
        v1 <- match(contactFrom[i],uniqueIDs)
        v2 <- match(contactTo[i],uniqueIDs)
        add.edge(y,v2,v1)
        }
    }
    ## dynamic network
    if(!is.null(contactStart)){
      if(is.null(contactEnd)){
        if(is.null(duration)) stop("Need to specify duration if contactEnd is missing")
        # single timestamps
        contactEnd <- contactStart+duration
      }
      for(i in 1:numedges){
        v1 <- match(contactFrom[i],uniqueIDs)
        v2 <- match(contactTo[i],uniqueIDs)
        add.edge(y,v2,v1)
        activate.edges(y,onset=contactStart[i],terminus=contactEnd[i],e=get.edgeIDs(y,v=v1,alter=v2,neighborhood="out"))
      }
    }
    x@contacts <- y
    return(x)
}) # end obkContacts constructor

####################
####  ACCESSORS ####
####################

######################
## get.nindividuals ##
######################
setMethod("get.nindividuals","obkContacts", function(x, ...){
	if(is.null(x@contacts)) return(0)
    return(x@contacts%n%"n")
})

######################
## get.individuals ##
######################
setMethod("get.individuals","obkContacts", function(x, ...){
    if(is.null(x@contacts)) return(NULL)
    return(network.vertex.names(x@contacts))
})

######################
#### get.ncontacts ###
######################
setMethod("get.ncontacts","obkContacts", function(x, ...){
    if(is.null(x@contacts)) return(0)
    return(network.edgecount(x@contacts))
})

######################
#### get.contacts ###
######################
setMethod("get.contacts","obkContacts", function(x, ...){
    if(is.null(x@contacts)) return(0)
    return(x@contacts)
})

######################
####  SHOW METHOD ####
######################

setMethod ("show", "obkContacts", function(object){
    nindividuals <- get.nindividuals(object)
    ncontacts <- get.ncontacts(object)
    #individualword <- ifelse(nindividuals>1, "individuals", "individual")
    #contactword <- ifelse(nindividuals>1, "individuals", "individual")
    contacts <- get.contacts(object)
    if(class(contacts)[1]=="network"){
      contacttype <- " Contacts = fixed"
    }
    else{
      contacttype <- " Contacts = dynamic"
    }
    cat(paste(" Number of individuals = ", nindividuals, "\n"," Number of contacts = ",ncontacts,"\n",contacttype,"\n",sep=""))
    if(ncontacts>0) print(object@contacts)
})



######################
####  PLOT METHOD ####
######################

setMethod ("plot", "obkContacts", function(x, y=NULL, labels=get.individuals(x), ...){
    plot(x@contacts, label=labels, ...)
    return(invisible())
})



##################
####  TESTING ####
##################
## NOTE: THIS MUST BE COMMENTED WHEN COMPILING/INSTALLING THE PACKAGE

## ## test constructor / show
## cf <- c("a", "b", "a", "c", "d")
## ct <- c("b", "c", "c", "d", "b")
## onset <- c(1, 2, 3, 4, 5)
## terminus <- c(1.2, 4, 3.5, 4.1, 6)
## oc.static <- new("obkContacts",cf,ct,FALSE) # static network
## oc.dynamic <- new("obkContacts",cf,ct,FALSE,onset,terminus)
## oc.static
## oc.dynamic
