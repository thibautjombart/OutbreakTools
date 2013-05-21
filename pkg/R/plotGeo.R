
## hack to remove the NOTE in R CMD check about:
## plotGeo: no visible binding for global variable ‘lon’
## plotGeo: no visible binding for global variable ‘lat’
if(getRversion() >= "2.15.1")  utils::globalVariables(c("lon","lat"))


## Function to plot cases on a map

plotGeo <- function(data,location='location',isLonLat=FALSE,zoom='auto',source='google',colorBy=c(),center=c()){
	#function to plot cases on a map
	#names gives the name of the column with location information
	#isLatLon indicates whether this is already in lon/lat format (if TRUE, there should be two columns in 'names', the first corresponding to Lon, the second to Lat)
	#zoom is used to specify the zoom level of the map
	#maps are retrieved from source
	#colorBy specifies column which should be used for coloring of nodes
	if(!isLonLat){
		#get the actual lon/lat
		lonlat=geocode(get.data(data,location))
	}
	else{
		lonlat=data.frame(get.data(data,location))
		#rename to standard names used later
		names(lonlat)=c('lon','lat')
	}
	if(!is.null(colorBy)){
		#add a column for coloring covariate
		lonlat$colorBy=get.data(data,colorBy)
	}

	if(is.null(center)) #if not specified, center on the middle of the points
		centerLonLat <- c(lon=(max(lonlat[,1],na.rm=T)+min(lonlat[,1],na.rm=T))/2,lat=(max(lonlat[,2],na.rm=T)+min(lonlat[,2],na.rm=T))/2)
	else{#center on the given individual
		no=which(get.individuals(data,'individuals')==center)#TODO there should be a standard function to do this?
		if(length(no)==0){
			print(warning(paste('individual',center,'not found')))
		}
		centerLonLat <- c(lon=lonlat[no,1],lat=lonlat[no,2])
	}

	#download the map,
	map <- get_map(centerLonLat, zoom = zoom,source=source)
	if(!is.null(colorBy))
		p <- ggmap(map) + geom_point(data=lonlat,aes(x = lon, y = lat,colour=colorBy))
	else
		p <- ggmap(map) + geom_point(data=lonlat,aes(x = lon, y = lat))
	#draw the image
	p
}


