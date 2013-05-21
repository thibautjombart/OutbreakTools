

###############
## make.phylo ##
###############

setMethod("make.phylo", "obkData", function(x, locus=NULL, result=c("obkData","multiPhylo"),
                                            model = "N", pairwise.deletion = FALSE, method=nj,
                                            color.by=c("sample","individual","date"), palette=NULL,
                                            plot=FALSE, ask=TRUE, ...){
    if(get.nlocus(x)==0){
        warning("No DNA sequences in the data.")
        return(NULL)
    }
    if(is.null(palette)){
        palette <- colorRampPalette(brewer.pal(11, "RdYlGn"))
    }
    result <- match.arg(result)

    ## GET DNA SEQUENCES ##
    if(is.null(locus)) locus <- 1:get.nlocus(x)
    N <- length(locus)

    ## GET DISTANCES ##
    D <- lapply(get.dna(x, locus=locus), function(e) dist.dna(e, model=model, pairwise.deletion = pairwise.deletion))

    ## GET TREES ##
    tre <- lapply(D, function(e) ladderize(method(e)))

    ## SET COLORS ##
    if(!is.null(x@samples)){
        ## get color info ##
        ## color.by <- color.by[1]
        ## if(color.by=="sample") col.name <- "sampleID"
        ## if(color.by=="individual") col.name <- "individualID"
        ## if(color.by=="date") col.name <- "date"
        color.by <- match.arg(color.by[1], names(x@samples))
        col.var <- x@samples[, color.by]
        names(col.var) <- x@samples$sequenceID
        if(inherits(col.var, "Date")){
            col.var <- as.numeric(difftime(col.var, min(col.var), unit="days"))
        }

        ## get colors ##
        if(is.factor(col.var) || is.character(col.var)) {
            col <- fac2col(col.var, col.pal=palette)
        } else {
            col <- num2col(col.var, col.pal=palette)
        }
        names(col) <- x@samples$sequenceID

        ## assign color to tips of all trees ##
        for(i in 1:N){
            lab <- tre[[i]]$tip.label
            tre[[i]]$tip.col <- col[lab]
        }

        ## make legend annot ##
        if(is.factor(col.var) || is.character(col.var)){
            leg.col <- fac2col(unique(col.var), col.pal=palette)
            leg.txt <- unique(col.var)
        } else {
            leg.col <- num2col(pretty(col.var), col.pal=palette, x.min=min(col.var, na.rm=TRUE), x.max=max(col.var,na.rm=TRUE))
            leg.txt <- pretty(col.var)
        }
    }


    ## PLOT (OPTIONAL) ##
    if(plot){
        par(ask=ask, xpd=TRUE)
        for(i in 1:N){
            plot(tre[[i]], tip.color=tre[[i]]$tip.col, ...)
            legend("topright", fill=leg.col, legend=leg.txt, title=color.by)
            title(paste("locus:", locus[i]))
        }
    }


    ## RETURN OBJECT ##
    class(tre) <- "multiPhylo"
    ## out$legend <- list(col=leg.col, txt=leg.txt)
    if(result=="multiPhylo") return(tre) # returned object is multiPhylo

    x@trees <- tre # returned object is obkData
    return(x)

}) # end make.phylo

