#' Function to plot phylogenies of the class 'ggphy'
#'
#' @param ggphy An object of the class "ggphy"
#' @param tip_labels Logical.Should tip labels be plotted?
#' @param tip_attribute Dataframe with at least two columns. One column must contain the tip labels, the remainings are tip attributes
#' @param var_tip_labels Character. The name of the column of tip_attribute that contains the tip labels.
#' @param var_tip_colour Character. The name of the column of tip_attribute that contains the attribute to be colour-codded.
#' @export
#' @author Anton Camacho
#' @examples see misc/plot_ggphy_test.R


plot_ggphy<-function(ggphy,tip_labels=F,tip_attribute=NULL,var_tip_labels=NULL,var_tip_colour=NULL){
	
#TODO: allow edge and node attributes and merge with df_edge and df_node
    
	df_tip<-ggphy[[1]]
	df_node<-ggphy[[2]]
	df_edge<-ggphy[[3]]
	
	is_x_date<-inherits(df_edge$x_beg,"Date")
	
	if(!is.null(tip_attribute) & !is.null(var_tip_labels)){
#merge df_tip with tip attributes
        tmp<-merge(df_tip,tip_attribute,by.x="label",by.y=var_tip_labels)
        df_tip<-tmp
	}
	
    
#theme_set(theme_grey())
	theme_old<-theme_update(
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank(),	panel.grid.major.y = element_blank(),
                            panel.grid.minor.y = element_blank())
    
	p<-ggplot(df_edge)
	p<-p+geom_segment(data=df_edge,aes(x=x_beg,xend=x_end,y=y_beg,yend=y_end),lineend="round")
	p<-p+scale_y_continuous("",labels=NULL)
	if(is_x_date)
        p<-p+scale_x_date("Time",labels=date_format("%Y"),minor_breaks="1 year")
        else
            p<-p+scale_x_continuous("Time")
            
            if(tip_labels){
                p<-p+geom_text(data=df_tip,aes(x=x,y=y,label=label),hjust=0)
            }
	
	if(!is.null(var_tip_colour)){
		p<-p+geom_point(data=df_tip,aes_string(x="x",y="y",colour=var_tip_colour))
	}
	
	print(p)
    
	theme_set(theme_old)
    
	return(p)	
}



