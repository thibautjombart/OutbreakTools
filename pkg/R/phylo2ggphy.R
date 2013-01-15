#' Function to convert phylogenies from the class 'phylo' to the class 'ggphy'
#'
#' @param phylo an object of the class "phylo"
#' @param tip_dates a vector containing the sample dates of the tip in "Date" format, the dates must be ordered like the tips
#' @param branch_unit the unit of the branch. Either "year", "month", "day" or "subst". If a time unit is provided, together with tip_dates, then the x-axis of the phylogeny will be in the Date format 
#' @export
#' @author Anton Camacho
#' @examples see misc/plot_ggphy_test.R

phylo2ggphy<-function(phylo,tip_dates=NULL,branch_unit="subst"){
	
	phy<-phylo
	has_node_label<-(!is.null(phy$node.label))
	
	N_tips<-length(phy$tip.label)
    if(!is.null(tip_dates) & length(tip_dates)!=N_tips){stop("tip_dates must be the same size as the number of tips if provided")}
    
	edge<-as.data.frame(phy$edge)
	names(edge)<-c("beg","end")
	
	#if edge.length is not provided, set all to 1 by default
	if(is.null(edge$length<-phy$edge.length)){edge$length<-rep(1,nrow(edge))}
	
    
#find root
	ind<-which(!edge$beg%in%edge$end)
	phy_root<-unique(edge$beg[ind])
	if(length(phy_root)!=1){
        cat(length(phy_root),"root(s) found!!\n")
        stop("Algorithm cannot handle more than one root in the phylo at the moment!")
	}
    
	if(!is.rooted(phy)){
		
#find outgroup
#ind<-which(edge$beg==phy_root & edge$end<=N_tips)
#outG<-edge$end[ind]
		phy<-root(phy,node=phy_root,r=T)
#remove outG label
#phy$tip.label<-phy$tip.label[-outG]
		
		if(has_node_label){
#add NA to the root label
            ind<-phy_root-N_tips
            tmp<-rep(NA,phy$Nnode)
            tmp[-ind]<-phy$node.label
            phy$node.label<-tmp
		}
	}
    
	Nt<-N_tips
	Nn<-phy$Nnode
	Ne<-nrow(phy$edge)
	Nnt<-Nn+Nt
	
	tip<-1:Nt
	node<-(Nt+1):Nnt
    
#get the y coord of nodes: start from tips and browse tree to the root
	y<-rep(NA,Nnt)
	order_tip<-edge$end[edge$end<=Nt]
	y[order_tip]<-tip
	cur_tip<-tip
	removed<-c()
	
	while(length(removed)<(Nnt-1)){
		
#how many nodes have two children among cur_tip
		ind<-which(edge$end%in%cur_tip)
		n_children<-table(edge$beg[ind])
		ind<-which(n_children>1)
		
		new_tip<-as.numeric(names(ind))
		old_tip<-edge$end[edge$beg%in%new_tip]
		
		y_new_tip<-sapply(new_tip,function(i){
			children<-edge$end[edge$beg==i]
			return(mean(y[children]))
		})
		
		y[new_tip]<-y_new_tip
		removed<-c(removed,old_tip)
		cur_tip<-cur_tip[!cur_tip%in%old_tip]
		cur_tip<-c(cur_tip,new_tip)
		
	}
	
#get x coord: start from root and browse tree until tips
	x<-rep(NA,Nnt)
	x[phy_root]<-0
	cur_node<-phy_root
	visited<-c(cur_node)
    
	while(length(visited)<Nnt){
		df1<-data.frame(beg=cur_node)
		df2<-merge(df1,edge)
		x[df2$end]<-x[df2$beg]+df2$length
		cur_node<-df2$end
		visited<-c(visited,cur_node)
	}
	
	
    if(!is.null(tip_dates) & branch_unit%in%c("year","month","day")){
        cat("X axis is converted into date\n")
#tip_date<-as.Date(extract_string(phy$tip.label,"_",2))
        df<-data.frame(tip,date=tip_dates,age=x[tip])
        time_unit=switch(branch_unit,year=365.25,month=30.5,day=1)
        root_date<-mean(df$date-df$age*time_unit)
#check that the new tip dates do not vary much from sample dates
#new_tip_date<-root_date+df$age*time_unit
#print(range(new_tip_date-df$date))
#x coord as date
        x<-root_date+x*time_unit
    }
    
    
#build data frame for nodes and tips
    if(has_node_label){
        df_node<-data.frame(node,x=x[node],y=y[node],label=phy$node.label)
    }else{
        df_node<-data.frame(node,x=x[node],y=y[node])
    }
    
    df_tip<-data.frame(tip,x=x[tip],y=y[tip],label=phy$tip.label)
    
#build segment data frame
#horizontal segments
    edge_h<-edge
    edge_h$direction<-"H"
    edge_h$x_beg<-x[edge_h$beg]
    edge_h$x_end<-x[edge_h$end]
    edge_h$y_beg<-y[edge_h$end]
    edge_h$y_end<-edge_h$y_beg
    
#vertical segments
    edge_v<-edge
    edge_v$direction<-"V"
    edge_v$x_beg<-x[edge_v$beg]
    edge_v$x_end<-edge_v$x_beg
    edge_v$y_beg<-y[edge_v$beg]
    edge_v$y_end<-y[edge_v$end]
    
#combine
    df_edge<-rbind(edge_h,edge_v)
    df_edge<-df_edge[,-which(names(df_edge)=="length")]
    
#if node.label!=NULL put label on edges
    if(has_node_label){
        tmp<-data.frame(end=node,label=phy$node.label)
        df_edge<-merge(df_edge,tmp,all.x=T)
    }
    
    return(list(df_tip,df_node,df_edge))
}