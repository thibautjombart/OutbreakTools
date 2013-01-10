

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
setGeneric("get.ID", function(x, ...) standardGeneric("get.ID"))

##################
## get.type ##
##################
## return the type of the object
setGeneric("get.type", function(x, ...) standardGeneric("get.type"))

##################
## get.characteristic ##
##################
## return the characteristic of the object
setGeneric("get.characteristic", function(x, ...) standardGeneric("get.characteristic"))

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



