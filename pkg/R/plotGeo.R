#' Function to plot cases on a map
#'
#' @param data the main obkData object
#' @param location the name of the column holding strings representing geographical location, or two columns with lon/lat coordinates
#' @param isLonLat specifies whether the data are already in lon/lat format
#' @param zoom level of zooming. Higher number gives smaller scale. 
#' @param source internet source to download maps
#' @param colorBy attribute to color the nodes by
#' @param center individualID of individual to put at the center of the map. If left empty, method will focus on center of all points
#' @export
#' @author Rolf Ypma
#' @examples #load a dataset on equine influenza
#' @examples data(HorseFlu)
#' @examples x <- new("obkData", individuals=HorseFlu$individuals)
#' @examples #plot the individuals on a map
#' @examples plotGeo(x,location=c('lon','lat'),T,zoom=8)
#' @examples 
#' @examples #color by sex
#' @examples plotGeo(x,location=c('lon','lat'),T,zoom=8,colorBy='sex')
#' @examples 
#' @examples #zoom in on the small cluster, by centering on individual '9'
#' @examples plotGeo(x,location=c('lon','lat'),T,colorBy='sex',zoom=12,center='9')
#' @examples 
#' @examples 
#' @examples #another example
#' @examples data(rabies)
#' @examples data=new('obkData',individuals=data.frame(individualID=1:177,transmissions))
#' @examples plotGeo(data,location=c('xinf','yinf'),isLonLat=T)
#' @examples plotGeo(data,location=c('xinf','yinf'),isLonLat=T,zoom=7,colorBy='pr')
#' @examples #not functional, but pretty
#' @examples plotGeo(data,location=c('xinf','yinf'),isLonLat=T,zoom=7,colorBy='source')

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


