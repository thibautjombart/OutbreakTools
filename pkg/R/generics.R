

###########################
#### GENERAL ACCESSORS ####
###########################

## only generic functions are defined here, and possibly default methods ##
## specific methods are defined in relevant files ##

#############
## get.dna ##
#############
## return a matrix of DNA sequences
setGeneric("get.dna", function(x, ...) standardGeneric("get.dna"))


################
## get.locus ##
################
## return the loci in the object
setGeneric("get.locus", function(x, ...) standardGeneric("get.locus"))


################
## get.nlocus ##
################
## return the number of loci in the object
setGeneric("get.nlocus", function(x, ...) standardGeneric("get.nlocus"))


################
## get.nsequences ##
################
## return the number of loci in the object
setGeneric("get.nsequences", function(x, ...) standardGeneric("get.nsequences"))


