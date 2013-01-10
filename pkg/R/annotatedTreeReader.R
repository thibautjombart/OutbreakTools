#
# @author Marc A. Suchard
#
# A class for reading Newick formatted trees with BEAST-style annotations
#

test.MAS.function = function(x) {
    cat(x)
}

# THE CODE BELOW COMES FROM 'ape'. MY GOAL IS TO DERIVE FROM THIS TO READ IN BEAST-STYLE ANNOTATIONS

parse.traits = function(text, header=FALSE) {

    if (header) {
      text = gsub(pattern="\\{",x=text,replacement="\\[")
      text = gsub(pattern="\\}",x=text,replacement="\\]")
    }
  
  # Pull out annotation
  text = regmatches(text,regexpr(pattern="\\[.*?\\]",text))
  # Remove leading and trailing delimitors
  text = substring(text,3,nchar(text)-1)  

  pattern = "(\"[^\"]*\"+|[^,=\\s]+)\\s*(=\\s*(\\{[^=]*\\}|\"[^\"]*\"+|[^,]+))?"
  
  rgx = gregexpr(pattern,text,perl=TRUE)
  n = length(attr(rgx[[1]],"match.length"))
  traits = list()
  start = attr(rgx[[1]],"capture.start")
  names = attr(rgx[[1]],"capture.names")
  length = attr(rgx[[1]],"capture.length")
  names = attr(rgx[[1]],"capture.names")
  for (i in 1:n) {
    s = start[i,3]
    e = s + length[i,3]
    if (substring(text,e,e) == ",") {
        e = e - 1
    }
    value = substring(text,s,e)    
    
    s = start[i,1]
    e = s + length[i,1]
    if (substring(text,e,e) == "=") {
        e = e - 1
    }
    key = substring(text,s,e)
    if (!is.na(as.numeric(value))) {
        value = as.numeric(value)
    }
    traits[[key]] = value
  }

 #   browser()
 
 print(traits)
    return(traits)
}

MAS.read.tree = function (file = "", text = NULL, tree.names = NULL, skip = 0, 
    comment.char = "#", keep.multi = FALSE, ...) 
{
    unname <- function(treetext) {
        nc <- nchar(treetext)
        tstart <- 1
        while (substr(treetext, tstart, tstart) != "(" && tstart <= 
            nc) tstart <- tstart + 1
        if (tstart > 1) 
            return(c(substr(treetext, 1, tstart - 1), substr(treetext, 
                tstart, nc)))
        return(c("", treetext))
    }
    
    if (!is.null(text)) {
        if (!is.character(text)) 
            stop("argument `text' must be of mode character")
        tree <- text
    }
    else {
        tree <- scan(file = file, what = "", sep = "\n", quiet = TRUE, 
            skip = skip, comment.char = comment.char, ...)
    }
    if (identical(tree, character(0))) {
        warning("empty character string.")
        return(NULL)
    }
    
    tree <- gsub("[ \n\t]", "", tree)
    tree <- unlist(strsplit(tree, NULL))
    y <- which(tree == ";")
    Ntree <- length(y)
    x <- c(1, y[-Ntree] + 1)
    if (is.na(y[1])) 
        return(NULL)
    STRING <- character(Ntree)
    for (i in 1:Ntree) STRING[i] <- paste(tree[x[i]:y[i]], sep = "", 
        collapse = "")
            
    tmp <- unlist(lapply(STRING, unname))
    tmpnames <- tmp[c(TRUE, FALSE)]
    STRING <- tmp[c(FALSE, TRUE)]
    if (is.null(tree.names) && any(nzchar(tmpnames))) 
        tree.names <- tmpnames
    colon <- grep(":", STRING)   
    
    if (!is.null(tree.names)) {
        tree.traits = lapply(tree.names, parse.traits, header=T)
    }
    
        browser()
    
    if (!length(colon)) {
        obj <- lapply(STRING, clado.build)
    }
    else if (length(colon) == Ntree) {
        obj <- lapply(STRING, tree.build)
    }
    else {
        obj <- vector("list", Ntree)
        obj[colon] <- lapply(STRING[colon], tree.build)
        nocolon <- (1:Ntree)[!1:Ntree %in% colon]
        obj[nocolon] <- lapply(STRING[nocolon], clado.build)
    }
    for (i in 1:Ntree) {
        ROOT <- length(obj[[i]]$tip.label) + 1
        if (sum(obj[[i]]$edge[, 1] == ROOT) == 1 && dim(obj[[i]]$edge)[1] > 
            1) 
            stop(paste("The tree has apparently singleton node(s): cannot read tree file.\n  Reading Newick file aborted at tree no.", 
                i))
    }
    if (Ntree == 1 && !keep.multi) 
        obj <- obj[[1]]
    else {
        if (!is.null(tree.names)) 
            names(obj) <- tree.names
        class(obj) <- "multiPhylo"
    }
    obj
}
