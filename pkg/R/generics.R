

###########################
#### GENERAL ACCESSORS ####
###########################

## NOTE:
## only generic functions are defined here, and possibly default methods
## specific methods are defined in relevant files

##################
## get.ID ##
##################
## return the ID of the object
setGeneric("get.id", function(x, ...) standardGeneric("get.id"))

##############
## get.type ##
##############
## return the type of the object
setGeneric("get.type", function(x, ...) standardGeneric("get.type"))

##################
## get.characteristic ##
##################
## return the characteristic of the object
setGeneric("get.characteristic", function(x, ...) standardGeneric("get.characteristic"))

##################
## get.start.date ##
##################
## return the start.date of the object
setGeneric("get.start.date", function(x, ...) standardGeneric("get.start.date"))

##################
## get.end.date ##
##################
## return the end.date of the object
setGeneric("get.end.date", function(x, ...) standardGeneric("get.end.date"))

##################
## get.date ##
##################
## return the date of the object
setGeneric("get.date", function(x, ...) standardGeneric("get.date"))


##################
## get.duration ##
##################
## return the duration of the object
setGeneric("get.duration", function(x, ...) standardGeneric("get.duration"))

#############
## get.dna ##
#############
## return a matrix of DNA sequences
setGeneric("get.dna", function(x, ...) standardGeneric("get.dna"))


###############
## get.locus ##
###############
## return the loci in the object
setGeneric("get.locus", function(x, ...) standardGeneric("get.locus"))


################
## get.nlocus ##
################
## return the number of loci in the object
setGeneric("get.nlocus", function(x, ...) standardGeneric("get.nlocus"))


####################
## get.nsequences ##
####################
## return the number of sequences in the object
setGeneric("get.nsequences", function(x, ...) standardGeneric("get.nsequences"))


##################
## get.nsamples ##
##################
## return the number of samples in the object
setGeneric("get.nsamples", function(x, ...) standardGeneric("get.nsamples"))


#################
## get.samples ##
#################
## return the number of samples in the object
setGeneric("get.samples", function(x, ...) standardGeneric("get.samples"))

####################
## get.sampletype ##
####################
setGeneric("get.sampletype",function(x, ...) standardGeneric("get.sampletype"))



######################
## get.nindividuals ##
######################
## return the number of individuals in the object
setGeneric("get.nindividuals", function(x, ...) standardGeneric("get.nindividuals"))


#####################
## get.individuals ##
#####################
## return the number of individuals in the object
setGeneric("get.individuals", function(x, ...) standardGeneric("get.individuals"))



###################
## get.ncontacts ##
###################
## return the number of contacts in the object
setGeneric("get.ncontacts", function(x, ...) standardGeneric("get.ncontacts"))


##################
## get.contacts ##
##################
## return the number of contacts in the object
setGeneric("get.contacts", function(x, ...) standardGeneric("get.contacts"))


##############
## get.data ##
##############
## return the number of contacts in the object
setGeneric("get.data", function(x, ...) standardGeneric("get.data"))



