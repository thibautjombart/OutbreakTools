##############################
## make.sequence.attributes ##
##############################
make.sequence.attributes <- function(x){
    ## check object type##
    if(!inherits(x,"obkData")) stop("x is not an obkData object")

    ## start with what is in @dna@meta ##
    out <- x@dna@meta

    ## merge with info on individuals (@individuals) ##
    if(!is.null(x@individuals)){
        df.individuals <- get.data(x, "individuals")
        df.individuals$individualID <- rownames(df.individuals)
        out <- merge(out, df.individuals, by = "individualID", all.x = TRUE)
    }

    ## merge with info on records (@records) ##
    if(!is.null(x@records)){
        ## merge each data.frame (by individualID)
        for(i in 1:length(x@records)){
            temp <- x@records[[i]]
            out <- merge(out, temp, by = "individualID", all.x = TRUE)
        }
    }

    ## clean the data.frame (removed by-products of the merge) ##
    names(out)[2] <- "date"

    return(out)
} # end make.sequence.attributes






#########################
## make.tip.attributes ##
#########################
make.tip.attributes <- function(x, which.tree = 1){
    ## check object type##
    if(!inherits(x,"obkData")) stop("x is not an obkData object")

    ## start with what is in @dna@meta ##
    phylo <- get.trees(x)[[which.tree]]
    out <- x@dna@meta[phylo$tip.label, , drop=FALSE]

    ## merge with info on individuals (@individuals) ##
    if(!is.null(x@individuals)){
        df.individuals <- get.data(x, "individuals")
        df.individuals$individualID <- rownames(df.individuals)
        out <- merge(out, df.individuals, by = "individualID", all.x = TRUE)
    }

    ## merge with info on records (@records) ##
    if(!is.null(x@records)){
        ## merge each data.frame (by individualID)
        for(i in 1:length(x@records)){
            temp <- x@records[[i]]
            out <- merge(out, temp, by = "individualID", all.x = TRUE)
        }
    }

    ## clean the data.frame (removed by-products of the merge) ##
    names(out)[2] <- "date"

    return(out)
} # end make.tip.attributes
