#epidase documentation for RData using roxygen2
#put you .RData in the /data directory
#add you roxygen2 description of your data below
#run roxygenize("./pkg") in the root directory of the epibase package

#' @name FluH1N1pdm2009
#' @title Dataset from the 2009 influenza A/H1N1 pandemic
#' @description This dataset is a list containing the following components:
#' \item{\code{individuals}}{A dataframe which contains 514 individual ID as well as their location}
#' \item{\code{samples}}{A dataframe which contains 514 individual ID, their sample ID and date as well as the ID of the associated genetic sequence}
#' \item{\code{dna}}{A \code{\link{DNAbin}} object containing 514 genetic sequences of 2009/H1N1 Haemaglutinin}
#' @author Anton Camacho
#' @docType data
NULL
