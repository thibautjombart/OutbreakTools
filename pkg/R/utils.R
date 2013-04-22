####################
## extract_string ##
####################
.extract.string <- function(v.string,my.split,position,from="first"){
	if(!from%in%c("first","last")){stop("incorrect \"from\" argument")}
	res<-sapply(v.string,function(x){
        tmp<-strsplit(as.character(x),split=my.split,fixed=T)[[1]]
        if(from=="last"){
        	position=length(tmp)-position+1
        }
        return(tmp[position])})
	return(res)
}


## #####################
## ## .findDateFormat ##
## #####################
## .findDateFormat <- function(x){
##     x <- as.character(x)[1L]
##     symb <- unlist(strsplit(gsub("[[:digit:]]","", x),""))[1L]
##     temp <- unlist(strsplit(x, paste("[",symb,"]",sep="")))

##     if(nchar(temp)[1]==4){
##         return(paste("%Y","%m","%d", sep=symb))
##     }

##     if(nchar(temp)[3]==4){
##         return(paste("%d","%m","%Y", sep=symb))
##     }

##     return("")
## }

###################
## .process.Date ##
###################

.process.Date <- function(x, format=NULL){
    if(inherits(x, "Date")) return(x)

    if(is.null(format)){

        bshape <- 0
        if(is.factor(x)) x <- as.character(x)
        x1 <- x[1]
        symb <- unlist(strsplit(gsub("[[:digit:]]","", x1),""))[1L]
        temp <- unlist(strsplit(x1, paste("[",symb,"]",sep="")))

        if(nchar(temp)[2]>2){
            ## months are written in letters
            ## needs to make sure the locale is fine
            bshape <- 1
            lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

            on.exit(Sys.setlocale("LC_TIME", lct))
        }

        if(nchar(temp)[1]==4){
            if(bshape)
                format <- paste("%Y","%b","%d", sep=symb)
            else
                format <- paste("%Y","%m","%d", sep=symb)
        }
        else if(nchar(temp)[3]==4){
            if(bshape)
                format <- paste("%d","%b","%Y", sep=symb)
            else
                format <- paste("%d","%m","%Y", sep=symb)
        }
        else{
            format <- ""
            warning(paste("date is provided in an ambiguous format\n"))
            return(as.Date(rep(NA, length(x))))
        }
    }

    return(as.Date(x,format=format))
} # end .process.Date
