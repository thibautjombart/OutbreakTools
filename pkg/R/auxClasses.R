
####################################################
####  S3 CLASSES DEFINED AS S4 AND CLASS UNIONS ####
####################################################

## DNA SEQUENCES
setOldClass("DNAbin")

## DATES
setOldClass("POSIXct")
setOldClass("Date")

## CONTACT NETWORKS
setOldClass("networkDynamic")
setOldClass("network")
setClassUnion("networkDynamicOrNetwork",c("network","networkDynamic"))

## ALLOW FOR SLOTS TO HAVE A TYPE, OR NULL
setClassUnion("characterOrNULL", c("character","NULL"))
setClassUnion("integerOrNULL", c("integer","NULL"))
setClassUnion("factorNULL", c("factor","NULL"))
setClassUnion("numericOrNULL", c("numeric","NULL"))
setClassUnion("matrixOrNULL", c("matrix","NULL"))
setClassUnion("listOrNULL", c("list","NULL"))
setClassUnion("DNAbinOrNULL", c("DNAbin","NULL"))
setClassUnion("POSIXctOrNULL", c("POSIXct","NULL"))
<<<<<<< HEAD
setClassUnion("networkDynamicOrNetworkOrNULL",c("networkDynamicOrNetwork","NULL"))

=======
setClassUnion("DateOrNULL", c("Date", "NULL"))
>>>>>>> 18446196d9bae2d9faae8502ae3abac38bd920d5
