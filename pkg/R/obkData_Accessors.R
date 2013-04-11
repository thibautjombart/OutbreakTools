
###############################
#### ACCESSORS FOR OBKDATA ####
###############################

###############
## get.locus ##
###############
setMethod("get.locus", "obkData", function(x, ...){
    if(is.null(x@dna)) return(NULL)
    return(get.locus(x@dna))
})



################
## get.nlocus ##
################
setMethod("get.nlocus", "obkData", function(x, ...){
    if(is.null(x@dna)) return(0)
    return(get.nlocus(x@dna))
})




###################
## get.sequences ## (get sequence ID)
###################
setMethod("get.sequences","obkData", function(x, ...){
    return(get.sequences(x@dna))
})




####################
## get.nsequences ##
####################
setMethod("get.nsequences", "obkData", function(x, ...){
    if(is.null(x@dna)) return(0)
    return(get.nsequences(x@dna))
})



#############
## get.dna ##
#############
setMethod("get.dna", "obkData", function(x, locus=NULL, id=NULL, ...){
    ## checks and escapes ##
    if(is.null(x@dna)) return(NULL)
    return(get.dna(x@dna, locus=locus, id=id, ...))
})



#####################
## get.individuals ##
#####################
setMethod("get.individuals", "obkData", function(x, data=c("samples", "individuals", "clinical", "contacts"), ...){
    data <- match.arg(data)

    ## list individuals in @samples
    if(data=="samples"){
        if(is.null(x@samples)) return(NULL)
        return(unique(x@samples$individualID))
    }

    ## list individuals in @individuals
    if(data=="individuals"){
        if(is.null(x@individuals)) return(NULL)
        return(row.names(x@individuals))
    }

    ## list individuals in @clinical
    if(data=="clinical"){
      if(is.null(x@clinical)) return(NULL)
      v_ind<-c()
      for(i in 1:length(x@clinical)){
        v_ind<-c(v_ind,x@clinical[[i]]$individualID)
      }
      return(unique(v_ind))
    }
	
	## list individuals in @contacts
	if(data=="contacts"){
		if(is.null(x@contacts)) return(NULL)
		return(get.individuals(x@contacts))
  	}
	
})



######################
## get.nindividuals ##
######################
setMethod("get.nindividuals", "obkData", function(x, data=c("samples", "individuals", "clinical", "contacts"), ...){
    data <- match.arg(data)

    return(length(get.individuals(x, data=data)))
})



#################
## get.samples ##
#################
setMethod("get.samples", "obkData", function(x, ...){
    if(is.null(x@samples)) return(NULL)
    return(unique(x@samples$sampleID))
})



#################
## get.nsamples ##
#################
setMethod("get.nsamples", "obkData", function(x, ...){
    if(is.null(x@samples)) return(0)
    return(length(unique(x@samples$sampleID)))
})



#################
## get.clinical ##
#################
setMethod("get.clinical", "obkData", function(x, ...){
  if(is.null(x@clinical)) return(NULL)
  # return the list of names of the different clinical tables
  return(unique(names(x@clinical)))
})



#################
## get.nclinicals ##
#################
setMethod("get.nclinicals", "obkData", function(x, ...){
  if(is.null(x@clinical)) return(0)
  # return the number of different clinical tables
  return(length(names(x@clinical))) 
})


#####################
## get.dates ##
#####################
setMethod("get.dates", "obkData", function(x, data=c("samples", "individuals", "clinical"), ...){
  data <- match.arg(data)
  
  ## list dates in @samples
  if(data=="samples"){
    if(is.null(x@samples$date)) return(NULL)
    return(unique(x@samples$date))
  }
  
  ## list dates in @individuals
  if(data=="individuals"){
    if(is.null(x@individuals$date)) return(NULL)
    return(x@individuals$date)
  }
  
  ## list dates in @clinical
  if(data=="clinical"){
    if(is.null(x@clinical)) return(NULL)
    v_dates<-c()
    for(i in 1:length(x@clinical)){
      v_dates<-c(v_dates,as.character(x@clinical[[i]]$date))
    }
    v_dates<-as.Date(v_dates,date.format= "%Y-%m-%d")
    return(unique(v_dates))
  }
})


#################
## get.trees ##
#################
setMethod("get.trees", "obkData", function(x, ...){
    return(x@trees)
})



##############
## get.data ##
##############
##
## Universal accessor:
## tries to find any type of data within the obkData object
##
setMethod("get.data", "obkData", function(x, data, where=NULL, drop=TRUE, ...){
    data <- as.character(data)

    ## LOOK FOR SLOT NAMES ##
    if(data[1] %in% slotNames(x)) return(slot(x, data))

    ## HANDLE 'WHERE'
    if(!is.null(where)){
        where <- match.arg(as.character(where), c("individuals","samples","clinical"))
        if(where=="individuals"){
            if(is.null(x@individuals)) { # return NULL if empty
                warning("x@individuals is NULL")
                return(NULL)
            }
            if(any(data %in% names(x@individuals))){
                return(x@individuals[,data,drop=drop])
            } else {
                warning(paste("data '", data, "'was not found in @individuals"))
                return(NULL)
            }
        } # end where==individuals

        if(where=="samples"){
            if(is.null(x@samples)) { # return NULL if empty
                warning("x@samples is NULL")
                return(NULL)
            }
            if(any(data %in% names(x@samples))){
                return(x@samples[,data,drop=drop])
            } else {
                warning(paste("data '", data, "'was not found in @samples"))
                return(NULL)
            }
        } # end where==samples

        if(where=="clinical"){
            if(is.null(x@clinical)) { # return NULL if empty
                warning("x@clinical is NULL")
                return(NULL)
            }
            for(i in 1:length(x@clinical)){
                if(any(data %in% names(x@clinical[[i]]))){
                    return(x@clinical[[i]][,data,drop=drop])
                }
            }
            warning(paste("data '", data, "'was not found in @clinical"))
            return(NULL)
        } # end where==clinical
    } # end if 'where' provided

    ## LOOK FOR 'DATA' IN INDIVIDUALS ##
    if(!is.null(x@individuals)){
        if(any(data %in% names(x@individuals))){
            return(x@individuals[,data,drop=drop])
        }
    }

    ## LOOK FOR 'DATA' IN SAMPLES ##
    if(!is.null(x@samples)){
        if(any(data %in% names(x@samples))){
            return(x@samples[,data,drop=drop])
        }
    }

    ## LOOK FOR 'DATA' IN CLINICAL ##
    if(!is.null(x@clinical)){
        for(i in 1:length(x@clinical)){
            if(any(data %in% names(x@clinical[[i]]))){
                return(x@clinical[[i]][,data,drop=drop])
            }
        }
    }


    ## DEFAULT IF WE DON'T KNOW WHAT TO RETURN ##
    warning(paste("data '", data, "'was not found in the object"))
    return(NULL)
}) # end get.data





