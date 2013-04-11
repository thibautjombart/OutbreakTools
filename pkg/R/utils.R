####################
## extract_string ##
####################
extract_string <- function(v_string,my_split,position){
	res<-sapply(v_string,function(x){
        tmp<-strsplit(as.character(x),split=my_split,fixed=T)[[1]]
        return(tmp[position])})
	return(res)
}



##################
## GeomSegment2 ##
##################
GeomSegment2 <- proto(ggplot2:::GeomSegment, {
	objname <- "geom_segment2"
	draw <- function(., data, scales, coordinates, arrow=NULL, ...) {
		if (is.linear(coordinates)) {
			return(with(coord_transform(coordinates, data, scales),
                        segmentsGrob(x, y, xend, yend, default.units="native",
                                     gp = gpar(col=alpha(colour, alpha), lwd=size * .pt,
                                               lty=linetype, lineend = "round"),
                                     arrow = arrow)
                        ))
        }
    }})



###################
## geom_segment2 ##
###################
geom_segment2 <- function(mapping = NULL, data = NULL, stat =
                          "identity", position = "identity", arrow = NULL, ...) {
	GeomSegment2$new(mapping = mapping, data = data, stat = stat,
                     position = position, arrow = arrow, ...)
}



#####################
## .findDateFormat ##
#####################
.findDateFormat <- function(x){
    x <- as.character(x)[1L]
    symb <- unlist(strsplit(gsub("[[:digit:]]","", x),""))[1L]
    temp <- unlist(strsplit(x, paste("[",symb,"]",sep="")))

    if(nchar(temp)[1]==4){
        return(paste("%Y","%m","%d", sep=symb))
    }

    if(nchar(temp)[3]==4){
        return(paste("%d","%m","%Y", sep=symb))
    }

    return("")
}

#####################
## .process.Date ##
#####################
.process.Date <- function(x, format=NULL){
  
  date.format <- format
  
  if(is.null(date.format)){
    
    bshape <- 0
    x <- as.character(x)[1L]
    symb <- unlist(strsplit(gsub("[[:digit:]]","", x),""))[1L]
    temp <- unlist(strsplit(x, paste("[",symb,"]",sep="")))
    
    if(nchar(temp)[2]>2){
      # months are written in letters
      # needs to make sure the locale is fine
      bshape <- 1
      lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
      
      on.exit(
        Sys.setlocale("LC_TIME", lct)
      )
    }
    
    if(nchar(temp)[1]==4){
      if(bshape)
        date.format <- paste("%Y","%b","%d", sep=symb)
      else
        date.format <- paste("%Y","%m","%d", sep=symb)
    }  
    else if(nchar(temp)[3]==4){
      if(bshape)
        date.format <- paste("%d","%b","%Y", sep=symb)
      else
        date.format <- paste("%d","%m","%Y", sep=symb)
    }
    else{
      date.format=""
      warning(paste("date is provided in an ambiguous format\n"))
      return(as.Date(rep(NA, length(x))))
    }
  }
  
  return(as.Date(x,format=date.format))
}
