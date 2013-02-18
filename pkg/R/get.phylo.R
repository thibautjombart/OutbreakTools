

###############
## get.phylo ##
###############

setMethod("get.phylo", "obkData", function(x, locus=NULL, model = "N", pairwise.deletion = FALSE, method=nj,
                                           color.by=c("sample","individual","date"), palette=funky, ...){
    if(is.null(get.dna(x))){
        warning("No DNA sequences in the data.")
        return(NULL)
    }

    ## GET DNA SEQUENCES ##
    if(is.null(locus)) locus <- 1:get.nlocus(x)
    N <- length(locus)

    ## GET DISTANCES ##
    D <- lapply(1:N, function(i) dist.dna(get.dna(x, locus=i), model=model, pairwise.deletion = pairwise.deletion))

    ## GET TREES ##
    tre <- lapply(D, method)

    ## SET COLORS ##
    if(!is.null(x@samples)){
        ## get color info ##
        if(color.by=="sample") col.name <- "sampleID"
        if(color.by=="individual") col.name <- "individualID"
        if(color.by=="date") col.name <- "date"
        col.var <- x@samples[, col.name]
        names(col.var) <- x@samples$sequenceID

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


    ## RETURN OBJECT ##
    out <- tre
    out$legend <- list(col=leg.col, txt=leg.txt)
    return(out)

})

