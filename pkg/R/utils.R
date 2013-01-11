extract_string<-function(v_string,my_split,position){
	res<-sapply(v_string,function(x){
        tmp<-strsplit(as.character(x),split=my_split,fixed=T)[[1]]
        return(tmp[position])})
	return(res)
}

