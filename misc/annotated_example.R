library(ape)

source("pkg/R/annotatedTreeReader.R")

system.time(a <- read.annotated.nexus(
    file="misc/H3_WD_n1000_GTRGI_E_skyline.trees_time_MCC.txt")
)
