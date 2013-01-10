ISOdate_format<-function(tab,type="d/m/y",year_max=2012){
    
	if(is.data.frame(tab)){
		n_col<-ncol(tab)
		single<-F
	}else{
		n_col<-1
		single<-T
	}
    
	for(i in 1:n_col){
		
		if(single){
			vect<-tab
		}else{
			vect<-tab[,i]
		}
		
		options(show.error.messages = FALSE)
		tab_i_ISO<-try(as.Date(vect))
		options(show.error.messages = TRUE)
		
		
		if(class(tab_i_ISO)=="try-error"){
		if(type=="numeric"){
			vect2<-as.numeric(as.character(vect))
			tab_i_ISO<-as.Date(vect2,origin="1970-01-01")
		}else{
            
#date pattern (order of day, month and year)
            pattern_date<-strsplit(type,split="/")[[1]]
            
#find first non-NA date to scan and detect format
            ind<-which((!is.na(vect)) & (vect!=""))
            test_date<-as.character(vect[ind[1]])
            
#find split
            tmp<-strsplit(test_date,split="")[[1]]
            if("-"%in%tmp){date_split<-"-"}
            if("/"%in%tmp){date_split<-"/"}
#find day
            pos_day<-which(pattern_date=="d")
            day_format<-"%d"
#find month: numeric or string?
            pos_month<-which(pattern_date=="m")
            tmp<-extract_string(test_date,date_split,pos_month)
            if(suppressWarnings(is.na(as.numeric(tmp)))){month_format<-"%b"}else{month_format<-"%m"}
#find year: 2 or 4 digits?
            pos_year<-which(pattern_date=="y")
            tmp<-extract_string(test_date,date_split,pos_year)
            if(nchar(tmp)==2){
                two_digit_year<-T
                year_format<-"%y"
            }else{
                two_digit_year<-F
                year_format<-"%Y"
            }
            
#create date_format
            date_element<-rep("",3)
            date_element[pos_year]<-year_format
            date_element[pos_month]<-month_format
            date_element[pos_day]<-day_format
            date_format<-paste(date_element,collapse=date_split)
            
            tab_i_ISO<-as.Date(vect,format=date_format)
            
            if(two_digit_year){
#check years
                tmp<-sapply(as.character(tab_i_ISO),function(x){
                    
                    if(is.na(x)){return(x)}
                    
                    x_year<-as.numeric(strftime(x,format="%Y"))
                    if(x_year>year_max){
                        x_year<-x_year-100
                        substr(x,1,4)<-as.character(x_year)
                    }
                    return(x)
                })
                
                tab_i_ISO<-as.Date(tmp)
                
            }
        }    
        }
        
        if(single){
            tab<-tab_i_ISO
        }else{
            tab[,i]<-tab_i_ISO
        }
        
	}
	return(tab)
}

