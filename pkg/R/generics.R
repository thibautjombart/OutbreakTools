

###########################
#### GENERAL ACCESSORS ####
###########################

## NOTE:
## only generic functions are defined here, and possibly default methods
## specific methods are defined in relevant files

############
## get.id ##
############
## return the id of the object
## setGeneric("get.id", function(x, ...) standardGeneric("get.id"))


#############
## get.dna ##
#############
## return DNA sequence alignments
setGeneric("get.dna", function(x, ...) standardGeneric("get.dna"))


#############
## get.ntrees ##
#############
## return multiPhylo object (list of trees)
setGeneric("get.ntrees", function(x, ...) standardGeneric("get.ntrees"))


#############
## get.trees ##
#############
## return multiPhylo object (list of trees)
setGeneric("get.trees", function(x, ...) standardGeneric("get.trees"))


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
## get.sequences ##
####################
## return the id of DNA sequences in the object
setGeneric("get.sequences", function(x, ...) standardGeneric("get.sequences"))


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

## ####################
## ## get.sampletype ##
## ####################
## setGeneric("get.sampletype",function(x, ...) standardGeneric("get.sampletype"))


#####################
## get.individuals ##
#####################
## return the individuals in the object
setGeneric("get.individuals", function(x, ...) standardGeneric("get.individuals"))



######################
## get.nindividuals ##
######################
## return the number of individuals in the object
setGeneric("get.nindividuals", function(x, ...) standardGeneric("get.nindividuals"))



#################
## get.samples ##
#################
## return the samples in the object
setGeneric("get.samples", function(x, ...) standardGeneric("get.samples"))



##################
## get.nsamples ##
##################
## return the number of samples in the object
setGeneric("get.nsamples", function(x, ...) standardGeneric("get.nsamples"))

#################
## get.clinicals ##
#################
## return the names of the clinical tables in the object
setGeneric("get.clinicals", function(x, ...) standardGeneric("get.clinicals"))


##################
## get.nclinicals ##
##################
## return the number of clinical tables in the object
setGeneric("get.nclinicals", function(x, ...) standardGeneric("get.nclinicals"))

#################
## get.dates ##
#################
## return the dates in the object
setGeneric("get.dates", function(x, ...) standardGeneric("get.dates"))


##################
## get.contacts ##
##################
## return the number of contacts in the object
setGeneric("get.contacts", function(x, ...) standardGeneric("get.contacts"))



###################
## get.ncontacts ##
###################
## return the number of contacts in the object
setGeneric("get.ncontacts", function(x, ...) standardGeneric("get.ncontacts"))



##############
## get.data ##
##############
## return the number of contacts in the object
setGeneric("get.data", function(x, ...) standardGeneric("get.data"))



############
## subset ##
############
## return the number of contacts in the object
setGeneric("subset", function(x, ...) standardGeneric("subset"))



###############
## make.phylo ##
###############
## return DNA sequence alignments
setGeneric("make.phylo", function(x, ...) standardGeneric("make.phylo"))
