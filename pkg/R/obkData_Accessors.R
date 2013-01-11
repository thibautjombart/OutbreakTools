###################
#### ACCESSORS ####
###################

################
## get.nlocus ##
################
setMethod("get.nlocus", "obkData", function(x, ...){
  if(is.null(x@dna)) return(0)
  sum(sapply(x@dna, get.nlocus))  
})

###############
## get.locus ##
###############
setMethod("get.locus", "obkData", function(x, ...){
  if(is.null(x@dna)) return(0)
  return(lapply(x@dna, get.locus))
})

####################
## get.nsequences ##
####################
setMethod("get.nsequences", "obkData", function(x, ...){
  if(is.null(x@dna)) return(0)
  return(sum(sapply(x@dna, get.nsequences)))
})

#############
## get.dna ##
#############
## setMethod("get.dna", "obkData", function(x, ...){
##   if(is.null(x@dna)) return(0)
##   return()
## })


##################### INDIVIDUAL LEVEL

######################
## get.nindividuals ##
######################
setMethod("get.nindividuals", "obkData", function(x, ...){
  if(is.null(x@individuals)) return(0)
  return(nrow(x@individuals))
})

#####################
## get.individuals ##
#####################
setMethod("get.individuals", "obkData", function(x, individual = NULL, ...){

  ## return NA if no info
  if(is.null(x@individuals)) return(NA)

  nInd <- get.nindividuals(x)
  ## return only individual if 
  if(!is.null(individual)){
   return(x)

   ## otherwise use information attached to each individual
   if(is.null(individual)) stop("individual must be specified (data contain more than one individual)")

   ## return new obkData object subsetted according to the individual ID
   ## return(new("obkData"))?
   
})
