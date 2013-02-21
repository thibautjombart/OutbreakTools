#' Function to plot phylogenies of the class 'ggphy'
#'
#' @param ggphy An object of the class "ggphy"
#' @param tip.labels Logical.Should tip labels be plotted?
#' @param tip.attribute Dataframe with at least two columns. One column must contain the tip labels, the remainings are tip attributes
#' @param var.tip.labels Character. The name of the column of tip.attribute that contains the tip labels.
#' @param var.tip.colour Character. The name of the column of tip.attribute that contains the attribute to be colour-codded.
#' @export
#' @author Anton Camacho
#' @examples see misc/plot.ggphy.test.R


plot.ggphy<-function(x, y=NULL, tip.labels=FALSE, tip.attribute=NULL, var.tip.labels=NULL, var.tip.colour=NULL, ...){

#TODO: allow edge and node attributes and merge with df.edge and df.node

	df.tip<-x[[1]]
	df.node<-x[[2]]
	df.edge<-x[[3]]

	is.x.date<-inherits(df.edge$x.beg,"Date")

	if(!is.null(tip.attribute) & !is.null(var.tip.labels)){
#merge df.tip with tip attributes
        tmp<-merge(df.tip,tip.attribute,by.x="label",by.y=var.tip.labels)
        df.tip<-tmp
	}


#theme.set(theme.grey())
	theme.old<-theme_update(
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank(),	panel.grid.major.y = element_blank(),
                            panel.grid.minor.y = element_blank())

	p<-ggplot(df.edge)
	p<-p+geom_segment(data=df.edge,aes(x=x.beg,xend=x.end,y=y.beg,yend=y.end),lineend="round")
	p<-p+scale_y_continuous("",labels=NULL)
	if(is.x.date)
        p<-p+scale_x_date("Time",labels=date.format("%Y"),minor.breaks="1 year")
        else
            p<-p+scale_x_continuous("Time")

            if(tip.labels){
                p<-p+geom_text(data=df.tip,aes(x=x,y=y,label=label),hjust=0)
            }

	if(!is.null(var.tip.colour)){
		p<-p+geom_point(data=df.tip,aes.string(x="x",y="y",colour=var.tip.colour))
	}

	print(p)

	theme_set(theme.old)

	return(p)
}



