
## hack to remove the NOTE in R CMD check about:
## plotGeo: no visible binding for global variable ‘lon’
## plotGeo: no visible binding for global variable ‘lat’
if(getRversion() >= "2.15.1")  utils::globalVariables(c("lon","lat"))


## Function to plot cases on a map

plotGeo <- function(x, location='location', zoom='auto', source='google',
                    colorBy=NULL, shapeBy=NULL, center=NULL, ...){
    ## function to plot cases on a map
    ## names gives the name of the column with location information
    ## isLatLon indicates whether this is already in lon/lat format (if TRUE, there should be two columns in 'names', the first corresponding to Lon, the second to Lat)
    ## zoom is used to specify the zoom level of the map
    ## maps are retrieved from source
    ## colorBy specifies column which should be used for coloring of nodes

    ## GET THE DATA ##
    if(is.null(x@individuals)) stop("no information on individuals - x@individuals is empty")
    isLonLat <- length(location)==2
    if(!isLonLat){
        ## get the actual lon/lat
        lonlat <- geocode(get.data(x, location, where="individuals"))
        df <- cbind.data.frame(get.data(x, "individuals"), lonlat)
    }
    else{
        df <- get.data(x, "individuals")
        ## rename to standard names used later
        lonlat <- df[location]
    }

    if(is.null(center)){ #if not specified, center on the middle of the points
        centerLonLat <- sapply(lonlat, mean, na.rm=TRUE)
    } else{#center on the given individual
        no=which(get.individuals(x,'individuals')==center)#TODO there should be a standard function to do this?
        if(length(no)==0){
            print(warning(paste('individual',center,'not found')))
        }
        centerLonLat <- c(lon=lonlat[no,1],lat=lonlat[no,2])
    }


    ## DOWNLOAD THE MAP ##
    map <- get_map(centerLonLat, zoom = zoom, source=source)

    ## GENERATE THE PLOT ##
    out <- ggmap(map) + geom_point(aes_string(x=location[1], y=location[2], colour=colorBy, shape=shapeBy), data=df, ...)

    ## draw the image
    return(out)
} # end plotGeo


