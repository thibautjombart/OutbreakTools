#roxygen commands
setwd("/Users/tonton/Documents/GitProjects/epibase/code/pkg/")
library(roxygen2)
DIR_PKG<-"/Users/tonton/Documents/GitProjects/epibase/code/pkg"
roxygenize(package=paste(DIR_PKG),roclets=c("collate","rd"))