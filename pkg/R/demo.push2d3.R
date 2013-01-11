
newcases.zoomseries=function(data){
	datachar=paste(colnames(data)[1],colnames(data)[2],sep=",")
	for(i in 1:nrow(data)){
		datachar=paste(datachar,paste(data[i,1],data[i,2],sep=","),sep="&#13;")
	}
	datachar
	html.link=postForm("http://www.spatialepidemiology.net/user_maps/php/demo.php",
           textarea = datachar)
	browseURL(html.link)
}

transmissions.circle=function(data,source,infected,group,level){
	index.source=(1:ncol(data))[colnames(data)==source]
	index.infected=(1:ncol(data))[colnames(data)==infected]
	index.group=(1:ncol(data))[colnames(data)==group]
	data.source=as.character(data[,index.source])
	data.infected=as.character(data[,index.infected])
	data.group=data[,index.group]
	options(useFancyQuotes = FALSE)
	link="["
	kglobal=0
	for(i in 1:nrow(data)){
		if(kglobal>0){
			link=paste(link,",",sep="")	
		}
		kglobal=kglobal+1
		namei=data.infected[i]
		linki=paste("{",dQuote("name"),":",
			dQuote(paste(data.group[i],".",namei,sep="")),",",
			dQuote("size"),":",level,",",
			dQuote("imports"),":","[",sep="")
		k=0
		for(j in (1:nrow(data))[!is.na(data.source)]){
			if(data.source[j]==namei){
				if(k>0){
					linki=paste(linki,",",sep="")	
				}
				k=k+1
				namej=data.infected[j]
				linki=paste(linki,
					dQuote(paste(data.group[j],".",namej,sep="")),
					sep="")
			}	
		}
		linki=paste(linki,"]}",sep="")
		link=paste(link,linki,sep="")
	}
	link=paste(link,"]",sep="")
	link=noquote(link)
	html.link=postForm("http://www.spatialepidemiology.net/user_maps/php/hierarchical.php",
           textarea = link)
	browseURL(html.link)  
}

#newcases.zoomseries(newcases)
#transmissions.circle(transmissions,source="source",infected="infected",group="Observed",level=1)

