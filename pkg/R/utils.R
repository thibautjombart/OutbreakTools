extract_string<-function(v_string,my_split,position){
	res<-sapply(v_string,function(x){
        tmp<-strsplit(as.character(x),split=my_split,fixed=T)[[1]]
        return(tmp[position])})
	return(res)
}

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


geom_segment2 <- function(mapping = NULL, data = NULL, stat =
                          "identity", position = "identity", arrow = NULL, ...) {
	GeomSegment2$new(mapping = mapping, data = data, stat = stat,
                     position = position, arrow = arrow, ...)
}

