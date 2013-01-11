
########################
####  BASIC METHODS ####
########################


#############
## show ##
#############

#setMethod ("show", "obkData", function(x){
# 	for (n in 1:6){
#  	 print(slotNames(x)[n],quote=FALSE)
#	show(slot(x,slotNames(x)[n]))
# })


#############
## summary ##
#############
setMethod("summary", "obkData", function(x, ...){
  for (n in 1:6){
  	 print(c("Summary of ", slotNames(x)[n]),quote=FALSE)
	print(summary(slot(x,slotNames(x)[n])))
  }
   return()
})












