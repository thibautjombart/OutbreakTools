##
## Used to display message at loading
##
.onAttach <- function(libname, pkgname){
    pkg.version <- packageDescription("hackout", fields = "Version")

    startup.txt <- paste(" hackout", pkg.version, "has been loaded\n")

    packageStartupMessage(startup.txt)
}
