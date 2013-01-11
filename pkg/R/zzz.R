##
## Used to display message at loading
##
.onAttach <- function(libname, pkgname){
    pkg.version <- packageDescription("epibase", fields = "Version")

    startup.txt <- paste(" epibase", pkg.version, "has been loaded\n")

    packageStartupMessage(startup.txt)
}
