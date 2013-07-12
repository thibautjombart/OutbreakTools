
###############################
#### ACCESSORS FOR OBKDATA ####
###############################

###############
## get.locus ##
###############
setMethod("get.locus", "obkData", function(x, ...){
    if(is.null(x@dna)) return(NULL)
    return(get.locus(x@dna, ...))
})



################
## get.nlocus ##
################
setMethod("get.nlocus", "obkData", function(x, ...){
    if(is.null(x@dna)) return(0)
    return(get.nlocus(x@dna, ...))
})




###################
## get.sequences ## (get sequence ID)
###################
setMethod("get.sequences","obkData", function(x, ...){
    return(get.sequences(x@dna, ...))
})




####################
## get.nsequences ##
####################
setMethod("get.nsequences", "obkData", function(x, ...){
    if(is.null(x@dna)) return(0)
    return(get.nsequences(x@dna, ...))
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
setMethod("get.individuals", "obkData", function(x, data=c("all", "individuals", "records", "contacts", "dna"), ...){
    data <- match.arg(data)

    ## list individuals in @individuals
    if(data=="individuals"){
        if(is.null(x@individuals)) return(NULL)
        return(row.names(x@individuals))
    }

    ## list individuals in @records
    if(data=="records"){
        if(is.null(x@records)) return(NULL)
        out <- unlist(lapply(x@records, function(e) e$individualID))
        return(out)
    }

    ## list individuals in @contacts
    if(data=="contacts"){
        if(is.null(x@contacts)) return(NULL)
        return(get.individuals(x@contacts))
    }

    ## list individuals in @contacts
    if(data=="dna"){
        if(is.null(x@dna)) return(NULL)
        return(get.individuals(x@dna))
    }


    ## list all individuals in the object (in @individuals, @records and @contacts)
    if(data=="all"){
        out <- unlist(lapply(c("individuals", "records", "contacts", "dna"), function(e) get.individuals(x, e)))
        return(out)
    }
})



######################
## get.nindividuals ##
######################
setMethod("get.nindividuals", "obkData", function(x, data=c("all", "individuals", "records", "contacts", "dna"), ...){
    data <- match.arg(data)

    return(length(unique(get.individuals(x, data=data))))
})




#################
## get.records ##
#################
setMethod("get.records", "obkData", function(x, ...){
  if(is.null(x@records)) return(NULL)
  return(names(x@records))
})



##################
## get.nrecords ##
##################
setMethod("get.nrecords", "obkData", function(x, ...){
  if(is.null(x@records)) return(0)
  return(length(get.records(x)))
})


#################
## get.context ##
#################
setMethod("get.context", "obkData", function(x, ...){
  if(is.null(x@context)) return(NULL)
  return(names(x@context))
})



##################
## get.nrecords ##
##################
setMethod("get.ncontext", "obkData", function(x, ...){
  if(is.null(x@context)) return(0)
  return(length(get.context(x)))
})


###############
## get.dates ##
###############
setMethod("get.dates", "obkData", function(x, data=c("all", "individuals", "records", "dna", "context"),...){

  data <- match.arg(data)

    ## list dates in @individuals
    if(data=="individuals"){
        if(is.null(x@individuals)) return(NULL)
        return(x@individuals$date)
    }

    ## list dates in @records
    if(data=="records"){
        if(is.null(x@records)) return(NULL)
        out <- unlist(lapply(x@records, function(e) e$date))
        return(out)
    }
    ## list dates in @context
    if(data=="context"){
      if(is.null(x@context)) return(NULL)
      out <- unlist(lapply(x@context, function(e) e$date))
      return(out)
    }
  
    ## list individuals in @contacts
    if(data=="dna"){
        if(is.null(x@dna)) return(NULL)
        return(get.dates(x@dna))
    }


    ## list all individuals in the object (in @individuals, @records and @contacts)
    if(data=="all"){
        out <- unlist(lapply(c("individuals", "records", "dna"), function(e) get.dates(x, e)))
        return(out)
    }
})



################
## get.ndates ##
################
setMethod("get.ndates", "obkData", function(x, data=c("all", "individuals", "records", "dna","context"),...){
    return(length(unique(get.dates(x, data=data))))
})



###############
## get.ntrees ##
###############
setMethod("get.ntrees", "obkData", function(x, ...){
    if(is.null(x@trees)) return(0L)
    return(length(x@trees))
})


###############
## get.trees ##
###############
setMethod("get.trees", "obkData", function(x, ...){
    return(x@trees)
})


##################
## get.contacts ##
##################
setMethod("get.contacts", "obkData", function(x, from=NULL, to=NULL, ...){
    if(is.null(x@contacts)) return(NULL)
    return(get.contacts(x@contacts, from=from, to=to, ...))
})



###################
## get.ncontacts ##
###################
setMethod("get.ncontacts", "obkData", function(x, from=NULL, to=NULL, ...){
    if(is.null(x@contacts)) return(0)
    return(get.ncontacts(x@contacts, from=from, to=to, ...))
})



##############
## get.data ##
##############
##
## Universal accessor:
## tries to find any type of data within the obkData object
##
setMethod("get.data", "obkData", function(x, data, where=NULL, drop=TRUE, showSource=FALSE, ...){
    data <- as.character(data)

    result <- data.frame()

    ## LOOK FOR SLOT NAMES ##
    if(data[1] %in% slotNames(x)) return(slot(x, data))

    ## HANDLE 'WHERE'
    if(!is.null(where)){
        where <- match.arg(as.character(where), c("individuals", "records", "context"))
    
        if(where=="individuals"){
            if(is.null(x@individuals)) { # return NULL if empty
                warning("x@individuals is NULL")
                return(NULL)
            }
            if(any(data %in% names(x@individuals))){
                temp<-x@individuals[,data,drop=F]
                temp<-cbind(temp,rownames(x@individuals))
                temp<-cbind(temp,rep("individuals",dim(temp)[1]))
                result<-temp
                names(result)<-c(data,"individualID","source")
            } else {
                warning(paste("data '", data, "'was not found in @individuals"))
                return(NULL)
            }
        } # end where==individuals

        if(where=="records"){
            if(is.null(x@records)) { # return NULL if empty
                warning("x@records is NULL")
                return(NULL)
            }
            ## look in @records ##
            if(any(data %in% names(x@records))){
                return(x@records[[data]])
            }
            ## look within slots in @records ##
            found <- FALSE
            for(i in 1:length(x@records)){
                if(any(data %in% names(x@records[[i]]))){
                    found <- TRUE
                    temp<-x@records[[i]][,c(data,"individualID")]
                    temp<-cbind(temp,rep(names(x@records)[i],dim(temp)[1]))
                    colnames(temp)<-c(data,"individualID","source")
                    ## colnames(temp)<-c(data[1],"individualID","source")
                }
                result<-rbind(result,temp)
            }
            if(!found){
                warning(paste("data '", data, "'was not found in @records"))
                return(NULL)
            }
        } # end where==records

        
        if(where=="context"){
          if(is.null(x@context)) { # return NULL if empty
            warning("x@context is NULL")
            return(NULL)
          }
          ## look in @context ##
          if(any(data %in% names(x@context))){
            return(x@context[[data]])
          }
          ## look within elements of @context
          found <- FALSE
          for(i in 1:length(x@context)){
            if(any(data %in% names(x@context[[i]]))){
              found=T
              temp<-x@context[[i]][,c(data,"date")]
              temp<-cbind(temp,rep(names(x@context)[i],dim(temp)[1]))
              colnames(temp)<-c(data,"date","source")
              
              result<-rbind(result,temp)
            }
          }
          if(!found){
            warning(paste("data '", data, "'was not found in @context"))
            return(NULL)
          }
        } # end where==context
        
    } # end if 'where' provided
    else{
        ## else, look everywhere

        ## LOOK FOR 'DATA' IN INDIVIDUALS ##
        if(!is.null(x@individuals)){
            if(any(data %in% names(x@individuals))){
                                        #temp<-x@individuals[,c(data,"individualID")]
                temp<-x@individuals[,data,drop=FALSE]
                temp<-cbind(temp,rownames(x@individuals))
                temp<-cbind(temp,rep("individuals",dim(temp)[1]))
                colnames(temp)<-c(data,"individualID","source")
                result<-temp
            }
        }


        ## LOOK FOR 'DATA' IN RECORDS ##
        if(!is.null(x@records)){
            ## look in @records ##
            if(any(data %in% names(x@records))){
                return(x@records[[data]])
            }

            ## look within slots in @records ##
            for(i in 1:length(x@records)){
                if(any(data %in% names(x@records[[i]]))){
                    temp<-x@records[[i]][,c(data,"individualID")]
                    temp<-cbind(temp,rep(names(x@records)[i],dim(temp)[1]))
                    colnames(temp)<-c(data,"individualID","source")
                    result<-rbind(result,temp)
                }
            }
        }
        ## LOOK FOR 'DATA' IN CONTEXT ##
        if(!is.null(x@context)){
          ## look in @context ##
          if(any(data %in% names(x@context))){
            return(x@context[[data]])
          }
          
          ## look within slots in @context ##
          for(i in 1:length(x@context)){
            if(any(data %in% names(x@context[[i]]))){
              temp<-x@context[[i]][,c(data,"date")]
              temp<-cbind(temp,rep(names(x@context)[i],dim(temp)[1]))
              colnames(temp)<-c(data,"date","source")
              result<-rbind(result,temp)
            }
          }
        }
    }
    if(length(result)>0){
        if(showSource)
            return(result)
        else
            return(result[,data,drop=drop])
    }
    else{
        ## DEFAULT IF WE DON'T KNOW WHAT TO RETURN ##
        warning(paste("data '", data, "'was not found in the object"))
        return(NULL)
    }
}) # end get.data





