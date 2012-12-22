
#################################
####  DEFINITIONS OF CLASSES ####
#################################

##################
## outbreak class
##################
setOldClass("DNAbin")
setOldClass("POSIXct")
setClassUnion("matrixOrNULL", c("matrix","NULL"))
setClassUnion("DNAbinOrNULL", c("DNAbin","NULL"))
setClassUnion("date", c("POSIXct","numeric","integer","NULL"))
setClass("outbreak", representation(swabs="matrixOrNULL", swab.case="character", swab.type="character", dna="DNAbinOrNULL", dna.case="character", dna.dates="date"),
                                    prototype(swabs=NULL, swab.case=character(0), swab.type=character(0), dna=NULL, dna.case=character(0), dna.dates=NULL))







#######################
####  CONSTRUCTORS ####
#######################

########################
## outbreak constructor
########################
setMethod("initialize", "outbreak", function(.Object, ...) {
    x <- .Object
    input <- list(...)
    ## if(length(input)==1) names(input) <- "snp"
    ## if(length(input)>1 && ! "snp" %in% names(input)) names(input)[1] <- "snp"

    ## handle arguments ##
    ## ...
    return(x)
}) # end SNPbin constructor









####################
####  ACCESSORS ####
####################

## model:
## setGeneric("foo", function(x, ...) standardGeneric("foo"))
## setGeneric("foo<-", function(x, value) standardGeneric("foo<-"))
##
##setMethod("foo","outbreak", function(x,...){
##    return(x@foo)
## })
##
##
## setReplaceMethod("foo","outbreak",function(x,value) {
##     if(is.null(value)){
##         slot(x, "foo", check=TRUE) <- value
##         return(x)
##     }
##    ## tests on value
##    ## ...
##    slot(x,"foo",check=TRUE) <- value[1]
##    return(x)
## })




########################
####  BASIC METHODS ####
########################

## setMethod ("show", "outbreak", function(object){
## })




